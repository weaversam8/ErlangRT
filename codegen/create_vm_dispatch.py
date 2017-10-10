#!/usr/bin/env python3
# takes: genop.tab from erlang/otp
# Prints a Rust source file of VM dispatch, huge match statement with all
# opcodes
import erlangrt.genop as genop


def main():
    conf = genop.OTP20()
    tables = genop.OTPTables(conf)

    print("""\
//! Generated by codegen/create_vm_dispatch.py
//! Dispatch for all opcode types.
//! Config used: {otp} 
#![allow(dead_code)]

use beam::gen_op;
use beam::opcodes::*;
use defs::{{Word, DispatchResult}};
use emulator::code::opcode::RawOpcode;
use emulator::heap::Heap;
use emulator::runtime_ctx::Context;


#[inline(always)]
pub fn dispatch_op_inline(op: RawOpcode, ctx: &mut Context, \
heap: &mut Heap) -> DispatchResult {{
  match op {{
""".format(op_max=conf.max_opcode, otp=conf.__class__.__name__))

    for opcode in range(conf.min_opcode, conf.max_opcode + 1):
        op = tables.ops[opcode]
        if op.name in tables.implemented_ops:
            print("    gen_op::OPCODE_{opcode} => "
                  "{{ return opcode_{lowercase}(ctx, heap) }},"
                  "".format(opcode=op.name.upper(), lowercase=op.name))

    print("""\
    other => unknown_opcode(other, ctx),   
  }
  DispatchResult::Yield
}
""")


if __name__ == "__main__":
    main()
