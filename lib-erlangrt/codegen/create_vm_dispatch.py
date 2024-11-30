#!/usr/bin/env python3
# takes: genop.tab from erlang/otp
# Prints a Rust source file of VM dispatch, huge match statement with all
# opcodes

import erlangrt.genop as genop


def main():
    conf = genop.OTP27()
    tables = genop.OTPTables(conf)

    print("""\
//! Generated by `codegen/create_vm_dispatch.py`
//! Dispatch for all opcode types.
//! Config used: {otp}
#![allow(dead_code)]

use crate::{{
  beam::{{disp_result::*, gen_op::*, opcodes::*}},
  emulator::{{code::opcode::RawOpcode, process::Process, runtime_ctx::*, vm::VM}},
  fail::RtResult,
}};

#[inline]
pub fn dispatch_op_inline(vm: &mut VM, op: RawOpcode, ctx: &mut RuntimeContext, \
curr_p: &mut Process) -> RtResult<DispatchResult> {{
  match op {{""".format(op_max=conf.max_opcode, otp=conf.__class__.__name__))

    for opcode in range(conf.min_opcode, conf.max_opcode + 1):
        op = tables.ops[opcode]
        if op.name in tables.implemented_ops:
            camelcased = "".join(
                map(lambda s: s.capitalize(), op.name.split("_"))
            )
            print("    OPCODE_{opcode} => {{\n"
                  "      assert_arity(OPCODE_{opcode}, Opcode{camelcased}::ARITY);\n"
                  "      return Opcode{camelcased}::__run(vm, ctx, curr_p);\n"
                  "    }},\n".format(opcode=op.name.upper(),
                            camelcased=camelcased))

    print("""\
    other => unknown_opcode(other, ctx),
  }
  Ok(DispatchResult::Yield(YieldType::EndOfTheQueue))
}
""")


if __name__ == "__main__":
    main()
