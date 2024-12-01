//! Module implements decoder for compact term format used in BEAM files.
//! <http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#beam-compact-term-encoding>

use crate::{
  beam::loader::CompactTermError,
  big,
  defs::{self, Word},
  emulator::heap::Heap,
  fail::{RtErr, RtResult},
  rt_util::bin_reader::BinaryReader,
  term::{
    boxed::{self, bignum::sign::Sign, endianness::Endianness},
    term_builder::{TupleBuilder, ListBuilder},
    Term,
  },
};

#[repr(u8)]
enum CteTag {
  LiteralInt = 0b000,
  Integer = 0b001,
  Atom = 0b010,
  XReg = 0b011,
  YReg = 0b100,
  Label = 0b101,
  Character = 0b110,
  Extended = 0b111,
}

#[cfg(feature = "r19")]
#[repr(u8)]
enum CteExtTag {
  Float = 0b0001_0111,
  List = 0b0010_0111,
  FloatReg = 0b0011_0111,
  AllocList = 0b0100_0111,
  Literal = 0b0101_0111,
}

// In OTP20 the Float Ext tag is gone and Lists are taking the first value
#[cfg(any(feature = "r20", feature = "r21", feature = "r22"))]
#[repr(u8)]
enum CteExtTag {
  List = 0b0001_0111,
  FloatReg = 0b0010_0111,
  AllocList = 0b0011_0111,
  Literal = 0b0100_0111,
}

#[cfg(any(feature = "r25", feature = "r26", feature = "r27"))]
#[repr(u8)]
enum CteExtTag {
  List = 0b0001_0111,
  FloatReg = 0b0010_0111,
  AllocList = 0b0011_0111,
  Literal = 0b0100_0111,
  RegTypeHint = 0b0101_0111,
}

// Different types 
#[repr(isize)]
enum CteAllocListType {
  Words = 0,
  Floats = 1,
  Funs = 2,
}

/// This defines how the read code will handle `CteExtTag::List`, either a jump
/// table can be built (assumes that the jump table will contain values and label
/// locations), or a `put_tuple2` initializer can be created (i.e. loaded as a
/// regular tuple).
pub enum ListParseMode {
  AsJumpTable,
  AsTuple2Initializer,
}

fn module() -> &'static str {
  "loader/cte: "
}

/// Defines the context for reading Compact Term Format. Reader is the source
/// of bytes; Heap is where larger terms are created; Mode defines the
/// behaviour when an `CteExtTag::List` has occured.
pub struct CompactTermReader {
  pub heap: *mut Heap,
  pub mode: ListParseMode,
}

impl CompactTermReader {
  pub fn new(hp: &mut Heap) -> Self {
    Self {
      heap: hp,
      mode: ListParseMode::AsJumpTable,
    }
  }

  /// If true, will create jumptables if ExtList tag has occured. A jumptable is
  /// checked to have labels at even positions. Otherwise will
  /// create an initializer tuple (where register values are allowed)
  #[inline]
  pub fn on_ext_list_create_jumptable(&mut self, b: bool) {
    if b {
      self.mode = ListParseMode::AsJumpTable
    } else {
      self.mode = ListParseMode::AsTuple2Initializer
    }
  }

  #[inline]
  fn make_err<T>(e: CompactTermError) -> RtResult<T> {
    Err(RtErr::CodeLoadingCompactTerm(e))
  }

  pub fn read(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    let b = reader.read_u8();
    let tag = b & 0b111;

    let bword = if tag < CteTag::Extended as u8 {
      self.read_word(reader, b)?
    } else {
      Term::make_small_unsigned(0)
    };

    match tag {
      x if x == CteTag::LiteralInt as u8 => {
        if bword.is_small() {
          return Ok(bword);
        }
        Self::make_err(CompactTermError::LiteralTag)
      }
      x if x == CteTag::Atom as u8 => {
        if bword.is_small() {
          let index = bword.get_small_unsigned();
          if index == 0 {
            return Ok(Term::nil());
          }
          return Ok(Term::make_loadtime_atom(index));
        }
        Self::make_err(CompactTermError::AtomTag)
      }
      x if x == CteTag::XReg as u8 => {
        if bword.is_small() {
          return Ok(Term::make_register_x(bword.get_small_unsigned()));
        }
        Self::make_err(CompactTermError::XRegTag)
      }
      x if x == CteTag::YReg as u8 => {
        if bword.is_small() {
          return Ok(Term::make_register_y(bword.get_small_unsigned()));
        }
        Self::make_err(CompactTermError::YRegTag)
      }
      x if x == CteTag::Label as u8 => {
        if bword.is_small() {
          return Ok(Term::make_loadtime_label(bword.get_small_unsigned()));
        }
        Self::make_err(CompactTermError::LabelTag)
      }
      x if x == CteTag::Integer as u8 => {
        // Can return small or big
        Ok(bword)
      }
      x if x == CteTag::Character as u8 => {
        if bword.is_small() {
          return Ok(bword);
        }
        Self::make_err(CompactTermError::CharacterTag)
      }
      // Extended tag (lower 3 bits = 0b111)
      _ => self.parse_ext_tag(reader, b),
    }
  }

  #[cfg(feature = "r19")]
  fn parse_ext_tag(&mut self, reader: &mut BinaryReader, b: u8) -> RtResult<LtTerm> {
    match b {
      x if x == CteExtTag::List as u8 => match self.mode {
        ListParseMode::AsJumpTable => self.parse_list_as_jump_table(reader),
        ListParseMode::AsTuple2Initializer => {
          self.parse_list_as_tuple_initializer(reader)
        }
      },
      x if x == CteExtTag::Float as u8 => self.parse_ext_float(),
      x if x == CteExtTag::FloatReg as u8 => self.parse_ext_fpreg(reader),
      x if x == CteExtTag::Literal as u8 => self.parse_ext_literal(reader),
      x if x == CteExtTag::AllocList as u8 => self.parse_alloc_list(reader),
      other => make_err(CompactTermError::ExtendedTag(format!(
        "Ext tag {} unknown",
        other
      ))),
    }
  }

  #[cfg(any(feature = "r20", feature = "r21", feature = "r22"))]
  fn parse_ext_tag(&mut self, reader: &mut BinaryReader, b: u8) -> RtResult<Term> {
    match b {
      x if x == CteExtTag::List as u8 => match self.mode {
        ListParseMode::AsJumpTable => self.parse_list_as_jump_table(reader),
        ListParseMode::AsTuple2Initializer => {
          self.parse_list_as_tuple_initializer(reader)
        }
      },

      // float does not exist after R19
      // x if x == CTEExtTag::Float as u8 => parse_ext_float(hp, r),
      x if x == CteExtTag::AllocList as u8 => self.parse_alloc_list(reader),
      x if x == CteExtTag::FloatReg as u8 => self.parse_ext_fpreg(reader),
      x if x == CteExtTag::Literal as u8 => self.parse_ext_literal(reader),
      other => {
        let msg = format!("Ext tag {other} unknown");
        Self::make_err(CompactTermError::ExtendedTag(msg))
      }
    }
  }

  #[cfg(any(feature = "r25", feature = "r26", feature = "r27"))]
  fn parse_ext_tag(&mut self, reader: &mut BinaryReader, b: u8) -> RtResult<Term> {
    match b {
      x if x == CteExtTag::List as u8 => match self.mode {
        ListParseMode::AsJumpTable => self.parse_list_as_jump_table(reader),
        ListParseMode::AsTuple2Initializer => {
          self.parse_list_as_tuple_initializer(reader)
        }
      },

      // float does not exist after R19
      // x if x == CTEExtTag::Float as u8 => parse_ext_float(hp, r),
      x if x == CteExtTag::AllocList as u8 => self.parse_alloc_list(reader),
      x if x == CteExtTag::FloatReg as u8 => self.parse_ext_fpreg(reader),
      x if x == CteExtTag::Literal as u8 => self.parse_ext_literal(reader),
      x if x == CteExtTag::RegTypeHint as u8 => self.parse_reg_type_hint(reader),
      other => {
        let msg = format!("Ext tag {other} unknown");
        Self::make_err(CompactTermError::ExtendedTag(msg))
      }
    }
  }

  /// Parses a tagged float. Note: Float tag does not exist after OTP 19
  #[cfg(feature = "r19")]
  fn parse_ext_float(&mut self) -> RtResult<Term> {
    // floats are always stored as f64
    let fp_bytes = reader.read_u64be();
    let float_val: f64 = unsafe { std::mem::transmute::<u64, f64>(fp_bytes) };
    unsafe { Term::make_float(&mut (*self.heap), float_val) }
  }

  fn parse_ext_fpreg(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    let b = reader.read_u8();
    let reg = self.read_word(reader, b)?;
    if reg.is_small() {
      return Ok(Term::make_register_float(reg.get_small_unsigned()));
    }
    let msg = "Ext tag FPReg value too big".to_string();
    Self::make_err(CompactTermError::ExtendedTag(msg))
  }

  fn parse_ext_literal(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    let b = reader.read_u8();
    let reg = self.read_word(reader, b)?;
    if reg.is_small() {
      return Ok(Term::make_loadtime_literal(reg.get_small_unsigned()));
    }
    let msg = "compact_term: loadtime Literal index too big".to_string();
    Self::make_err(CompactTermError::ExtendedTag(msg))
  }

  fn parse_list_as_tuple_initializer(
    &mut self,
    reader: &mut BinaryReader,
  ) -> RtResult<Term> {
    let arity = self.read_int(reader)? as usize;
    let tb = unsafe { TupleBuilder::with_arity(arity, &mut (*self.heap))? };

    for i in 0..arity {
      let value = self.read(reader)?;
      // No checkes whether a value is a register, because this is OK and allowed
      unsafe { tb.set_element(i, value) };
    }

    let result = tb.make_term();
    Ok(result)
  }

  /// Parses a list, places on the provided heap.
  /// Creates a jump table with even number of elements (values => locations).
  fn parse_list_as_jump_table(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    // The stream now contains a smallint size, then size/2 pairs of values
    let n_pairs = self.read_int(reader)? as usize / 2;
    let jt = unsafe { boxed::JumpTable::create_into(&mut (*self.heap), n_pairs)? };

    for i in 0..n_pairs {
      let value = self.read(reader)?;
      let loc = self.read(reader)?;
      unsafe {
        (*jt).set_pair(i, value, loc);
      }
    }

    Ok(Term::make_boxed(jt))
  }

  /// Parses an allocation list (used to specify how much memory to allocate)
  fn parse_alloc_list(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    // First, get the length, which should be a tag_u with a value of 3
    let len = self.read_int(reader)?;
    assert_eq!(len, 3, "compact_term: AllocList length must be 3");

    let mut lb = ListBuilder::new()?;

    // Next, parse the three values
    for _i in 0..len {
      // Read the type within the list
      let t = self.read_int(reader)?;
      match t {
        x if x == CteAllocListType::Words as isize => {
          let n_words = self.read_int(reader)?;
          unsafe {
            // TODO: I'm not positive this is right, I think element 0 should be an atom.
            // Leaving it be for now until I can get a little further along in the load
            // process.
            let tb = TupleBuilder::with_arity(2, &mut (*self.heap))?;
            tb.set_element(0, Term::make_small_signed(CteAllocListType::Words as isize));
            tb.set_element(1, Term::make_small_signed(n_words));
            lb.append(tb.make_term(), &mut (*self.heap))?;
          }
        }
        x if x == CteAllocListType::Floats as isize => {
          let n_floats = self.read_int(reader)?;
          unsafe {
            let tb = TupleBuilder::with_arity(2, &mut (*self.heap))?;
            tb.set_element(0, Term::make_small_signed(CteAllocListType::Floats as isize));
            tb.set_element(1, Term::make_small_signed(n_floats));
            lb.append(tb.make_term(), &mut (*self.heap))?;
          }
        }
        x if x == CteAllocListType::Funs as isize => {
          let n_funs = self.read_int(reader)?;
          unsafe {
            let tb = TupleBuilder::with_arity(2, &mut (*self.heap))?;
            tb.set_element(0, Term::make_small_signed(CteAllocListType::Funs as isize));
            tb.set_element(1, Term::make_small_signed(n_funs));
            lb.append(tb.make_term(), &mut (*self.heap))?;
          }
        }
        t => panic!("compact_term: AllocList unknown type {}", t),
      }
    }
    
    Ok(lb.make_term())
  }

  /// Parses an X or Y register annotated with type information. This feature
  /// was added in OTP 25.
  #[cfg(any(feature = "r25", feature = "r26", feature = "r27"))]
  fn parse_reg_type_hint(&mut self, reader: &mut BinaryReader) -> RtResult<Term> {
    // The first element will be a normal X or Y register, so it can be read as
    // normal:
    let reg = self.read(reader)?;

    // The second element will be a smallint, which I think is an index into
    // the type chunk of the file. This chunk is currently ignored, so we can
    // throw out this value.
    let b = reader.read_u8();
    let _tag: u8 = b & 0b111;
    let _type_idx = self.read_word(reader, b)?;

    Ok(reg)
  }

  /// Assume that the stream contains a tagged small integer (check the tag!)
  /// read it and return the unwrapped value as word.
  fn read_int(&mut self, reader: &mut BinaryReader) -> RtResult<isize> {
    let b = reader.read_u8();
    assert_eq!(b & 0b111, CteTag::LiteralInt as u8);
    let val = self.read_word(reader, b)?;
    if val.is_small() {
      return Ok(val.get_small_signed());
    }
    unimplemented!("unwrap bigint from term or return an error")
  }

  /// Given the first byte, parse an integer encoded after the 3-bit tag,
  /// read more bytes from stream if needed.
  fn read_word(&mut self, reader: &mut BinaryReader, b: u8) -> RtResult<Term> {
    if 0 == (b & 0b1000) {
      // Bit 3 is 0 marks that 4 following bits contain the value
      return Ok(Term::make_small_signed((b as isize) >> 4));
    }
    // Bit 3 is 1, but...
    if 0 == (b & 0b1_0000) {
      // Bit 4 is 0, marks that the following 3 bits (most significant) and
      // the following byte (least significant) will contain the 11-bit value
      let r = ((b as usize) & 0b1110_0000) << 3 | (reader.read_u8() as usize);
      Ok(Term::make_small_signed(r as isize))
    } else {
      // Bit 4 is 1 means that bits 5-6-7 contain amount of bytes+2 to store
      // the value
      let mut n_bytes = (b >> 5) as Word + 2;
      if n_bytes == 9 {
        // bytes=9 means upper 5 bits were set to 1, special case 0b11111xxx
        // which means that following nested tagged value encodes size,
        // followed by the bytes (Size+9)
        let bnext = reader.read_u8();
        let tmp = self.read_word(reader, bnext)?;
        if tmp.is_small() {
          n_bytes = tmp.get_small_unsigned() + 9;
        } else {
          panic!("{}read word encountered a wrong byte length", module())
        }
      }

      // Read the remaining big endian bytes and convert to int
      let long_bytes = reader.read_bytes(n_bytes)?;
      let sign = if long_bytes[0] & 0x80 == 0x80 {
        Sign::Negative
      } else {
        Sign::Positive
      };

      // Check if bytes are few enough to fit into a small integer
      // TODO: Can also do this when the length is equal to WORD_BYTES but then must check last byte bits to fit
      if long_bytes.len() < defs::WORD_BYTES {
        return Self::bytes_to_small(sign, &long_bytes);
      }

      let limbs = big::make_limbs_from_bytes(Endianness::Little, long_bytes);
      debug_assert!(
        !limbs.is_empty(),
        "Limbs vec can't be empty for creating a bigint"
      );
      let r = unsafe { boxed::Bignum::create_into(&mut (*self.heap), sign, &limbs)? };
      println!("Creating bigint with {limbs:?}");
      Ok(Term::make_boxed(r))
    } // if larger than 11 bits
  }

  fn bytes_to_small(sign: Sign, long_bytes: &[u8]) -> RtResult<Term> {
    let mut n = 0isize;
    // Assume little endian, so each next digit costs more
    let mut shift = 0usize;
    for digit in long_bytes {
      n |= (*digit as isize) << shift;
      shift += defs::BYTE_BITS;
    }
    if sign == Sign::Negative {
      n = -n;
    }
    Ok(Term::make_small_signed(n))
  }
}
