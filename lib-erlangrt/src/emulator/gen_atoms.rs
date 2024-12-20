//! Generated by `codegen/create_gen_atoms.py`
//! Creates array of predefined atoms
//! Config used: OTP25
#![allow(dead_code)]

use crate::term::*;


pub const SYM_PLUS: Term = Term::make_atom(0);
pub const SYM_MINUS: Term = Term::make_atom(1);
pub const SYM_EQ_EQ: Term = Term::make_atom(2);
pub const ALL: Term = Term::make_atom(3);
pub const APPLY: Term = Term::make_atom(4);
pub const BADARG: Term = Term::make_atom(5);
pub const BADARITH: Term = Term::make_atom(6);
pub const BADARITY: Term = Term::make_atom(7);
pub const BADFUN: Term = Term::make_atom(8);
pub const BADMATCH: Term = Term::make_atom(9);
pub const CASE_CLAUSE: Term = Term::make_atom(10);
pub const ERLANG: Term = Term::make_atom(11);
pub const ERROR: Term = Term::make_atom(12);
pub const ERTS_INTERNAL: Term = Term::make_atom(13);
pub const EXIT: Term = Term::make_atom(14);
pub const FALSE: Term = Term::make_atom(15);
pub const FUNCTION_CLAUSE: Term = Term::make_atom(16);
pub const HIGH: Term = Term::make_atom(17);
pub const IF_CLAUSE: Term = Term::make_atom(18);
pub const INIT: Term = Term::make_atom(19);
pub const KILL: Term = Term::make_atom(20);
pub const KILLED: Term = Term::make_atom(21);
pub const LOW: Term = Term::make_atom(22);
pub const NIF_ERROR: Term = Term::make_atom(23);
pub const NOCATCH: Term = Term::make_atom(24);
pub const NORMAL: Term = Term::make_atom(25);
pub const OK: Term = Term::make_atom(26);
pub const SYSTEM_LIMIT: Term = Term::make_atom(27);
pub const THROW: Term = Term::make_atom(28);
pub const TRAP_EXIT: Term = Term::make_atom(29);
pub const TRUE: Term = Term::make_atom(30);
pub const UNDEF: Term = Term::make_atom(31);
pub const UNDEFINED: Term = Term::make_atom(32);

pub static ATOM_INIT_NAMES: &'static [&'static str] = &[
  "+", // id=0
  "-", // id=1
  "==", // id=2
  "all", // id=3
  "apply", // id=4
  "badarg", // id=5
  "badarith", // id=6
  "badarity", // id=7
  "badfun", // id=8
  "badmatch", // id=9
  "case_clause", // id=10
  "erlang", // id=11
  "error", // id=12
  "erts_internal", // id=13
  "exit", // id=14
  "false", // id=15
  "function_clause", // id=16
  "high", // id=17
  "if_clause", // id=18
  "init", // id=19
  "kill", // id=20
  "killed", // id=21
  "low", // id=22
  "nif_error", // id=23
  "nocatch", // id=24
  "normal", // id=25
  "ok", // id=26
  "system_limit", // id=27
  "throw", // id=28
  "trap_exit", // id=29
  "true", // id=30
  "undef", // id=31
  "undefined", // id=32
];
