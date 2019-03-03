use crate::{
  defs::{BitSize, ByteSize, Word},
  fail::{RtErr, RtResult},
  term::{
    boxed::{
      binary::{binaryheap_bin::BinaryHeapBinary, trait_interface::TBinary, BinaryType},
      Binary,
    },
    lterm::LTerm,
  },
};
use crate::defs::BitDataPointer;

/// Defines operations with reference to binary.
/// Pointer to this can be directly casted from pointer to boxed::Binary
pub struct ReferenceToBinary {
  pub bin_header: Binary,
  pub size: BitSize,
  refc: Word,
  pub pointer: *mut BinaryHeapBinary,
}

impl ReferenceToBinary {
  #[allow(dead_code)]
  pub unsafe fn on_destroy(this: *mut ReferenceToBinary) {
    if (*this).refc > 0 {
      (*this).refc -= 1;
      return;
    }
  }
}

impl TBinary for ReferenceToBinary {
  fn get_type(&self) -> BinaryType {
    BinaryType::RefToBinaryHeap
  }

  fn get_byte_size(&self) -> ByteSize {
    self.size.get_bytes_rounded_up()
  }

  fn get_bit_size(&self) -> BitSize {
    self.size
  }

  unsafe fn get_data(&self) -> &[u8] {
    core::slice::from_raw_parts(core::ptr::null(), 0)
  }

  unsafe fn get_data_mut(&mut self) -> &mut [u8] {
    core::slice::from_raw_parts_mut(core::ptr::null_mut(), 0)
  }

  fn get_data_bitptr(&self) -> BitDataPointer {
    unimplemented!()
  }

  fn store(&mut self, _data: &[u8]) -> RtResult<()> {
    // TODO: Maybe should be possible? Assist with resolution into BinaryHeapBinary
    return Err(RtErr::CannotCopyIntoRefbin);
  }

  fn make_term(&self) -> LTerm {
    LTerm::make_boxed((&self.bin_header) as *const Binary)
  }
}
