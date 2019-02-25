//! Generated by `codegen/create_gen_atoms.py`
//! Creates array of predefined atoms
//! Config used: OTP22
#![allow(dead_code)]

use crate::term::lterm::*;


pub const SYM_PLUS: LTerm = LTerm::make_atom(0);
pub const SYM_MINUS: LTerm = LTerm::make_atom(1);
pub const SYM_EQ_EQ: LTerm = LTerm::make_atom(2);
pub const APPLY: LTerm = LTerm::make_atom(3);
pub const BADARG: LTerm = LTerm::make_atom(4);
pub const BADARITH: LTerm = LTerm::make_atom(5);
pub const BADARITY: LTerm = LTerm::make_atom(6);
pub const BADFUN: LTerm = LTerm::make_atom(7);
pub const BADMATCH: LTerm = LTerm::make_atom(8);
pub const CASE_CLAUSE: LTerm = LTerm::make_atom(9);
pub const ERLANG: LTerm = LTerm::make_atom(10);
pub const ERROR: LTerm = LTerm::make_atom(11);
pub const ERTS_INTERNAL: LTerm = LTerm::make_atom(12);
pub const EXIT: LTerm = LTerm::make_atom(13);
pub const FALSE: LTerm = LTerm::make_atom(14);
pub const FUNCTION_CLAUSE: LTerm = LTerm::make_atom(15);
pub const HIGH: LTerm = LTerm::make_atom(16);
pub const IF_CLAUSE: LTerm = LTerm::make_atom(17);
pub const INIT: LTerm = LTerm::make_atom(18);
pub const KILL: LTerm = LTerm::make_atom(19);
pub const KILLED: LTerm = LTerm::make_atom(20);
pub const LOW: LTerm = LTerm::make_atom(21);
pub const NIF_ERROR: LTerm = LTerm::make_atom(22);
pub const NOCATCH: LTerm = LTerm::make_atom(23);
pub const NORMAL: LTerm = LTerm::make_atom(24);
pub const OK: LTerm = LTerm::make_atom(25);
pub const SYSTEM_LIMIT: LTerm = LTerm::make_atom(26);
pub const THROW: LTerm = LTerm::make_atom(27);
pub const TRAP_EXIT: LTerm = LTerm::make_atom(28);
pub const TRUE: LTerm = LTerm::make_atom(29);
pub const UNDEF: LTerm = LTerm::make_atom(30);
pub const UNDEFINED: LTerm = LTerm::make_atom(31);

pub static ATOM_INIT_NAMES: &'static [&'static str] = &[
  "+", // id=0
  "-", // id=1
  "==", // id=2
  "apply", // id=3
  "badarg", // id=4
  "badarith", // id=5
  "badarity", // id=6
  "badfun", // id=7
  "badmatch", // id=8
  "case_clause", // id=9
  "erlang", // id=10
  "error", // id=11
  "erts_internal", // id=12
  "exit", // id=13
  "false", // id=14
  "function_clause", // id=15
  "high", // id=16
  "if_clause", // id=17
  "init", // id=18
  "kill", // id=19
  "killed", // id=20
  "low", // id=21
  "nif_error", // id=22
  "nocatch", // id=23
  "normal", // id=24
  "ok", // id=25
  "system_limit", // id=26
  "throw", // id=27
  "trap_exit", // id=28
  "true", // id=29
  "undef", // id=30
  "undefined", // id=31
];
