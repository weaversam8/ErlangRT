//! Module implements binary/bit syntax matching and data creation & extraction
//! opcodes for binaries.
pub mod bs_get_binary;
pub mod bs_init;
pub mod bs_put_integer;
pub mod bs_start_match;

pub use super::{bs_get_binary::*, bs_init::*, bs_put_integer::*, bs_start_match::*};

use crate::{
  beam::disp_result::DispatchResult,
  emulator::{process::Process, runtime_ctx::Context},
  fail::RtResult,
  term::{boxed::binary::match_state::BinaryMatchState, lterm::*},
};

// Values used in bs_* opcodes for flags
// pub const BSF_ALIGNED: usize = 1;
// pub const BSF_LITTLE: usize = 2; )
// pub const BSF_SIGNED: usize = 4;
// pub const BSF_EXACT: usize = 8;
// pub const BSF_NATIVE: usize = 16;
bitflags! {
    pub struct BsFlags: usize {
        const ALIGNED = 0b00000001; // Field is guaranteed to be byte-aligned
        const LITTLE  = 0b00000010; // Field is little-endian (otherwise big)
        const SIGNED  = 0b00000100; // Field is signed (otherwise unsigned)
        const EXACT   = 0b00001000; // Size in bs_init is exact
        const NATIVE  = 0b00010000; // Native endian
    }
}

// Having started binary matching, check that the match state has so many `Bits`
// remaining otherwise will jump to the `Fail` label.
// Structure: bs_test_tail2(Fail, MatchState, Bits)
define_opcode!(
  _vm, rt_ctx, proc, name: OpcodeBsTestTail2, arity: 3,
  run: { Self::bs_test_tail2(rt_ctx, proc, fail, match_state, bits) },
  args: cp_or_nil(fail), binary_match_state(match_state), load_usize(bits),
);


impl OpcodeBsTestTail2 {
  #[inline]
  fn bs_test_tail2(
    runtime_ctx: &mut Context,
    _proc: &mut Process,
    fail: Term,
    match_state: *mut BinaryMatchState,
    bits: usize,
  ) -> RtResult<DispatchResult> {
    let remaining = unsafe { (*match_state).get_bits_remaining().bit_count };
    if remaining != bits {
      runtime_ctx.jump(fail);
    }
    return Ok(DispatchResult::Normal);
  }
}

// This instruction is rewritten on Erlang/OTP to `move S2, Dst`
// Structure: bs_add(Fail, S1_ignored, S2, Unit, Dst)
define_opcode!(
  _vm, rt_ctx, proc, name: OpcodeBsAdd, arity: 5,
  run: {
    rt_ctx.store_value(s2, dst, &mut proc.heap)?;
    Ok(DispatchResult::Normal)
  },
  args: cp_or_nil(fail), IGNORE(s1), load(s2), IGNORE(unit), term(dst),
);
