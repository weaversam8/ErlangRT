//! Generated by `codegen/create_gen_atoms.py`
//! Creates array of predefined atoms
//! Config used: OTP20
#![allow(dead_code)]

use crate::term::lterm::*;


pub const STIMES_2: LTerm = LTerm::make_atom(0);
pub const SYM_PLUS: LTerm = LTerm::make_atom(1);
pub const EBIF_PLUSPLUS_2: LTerm = LTerm::make_atom(2);
pub const SYM_MINUS: LTerm = LTerm::make_atom(3);
pub const SNEQEQ_2: LTerm = LTerm::make_atom(4);
pub const SLT_2: LTerm = LTerm::make_atom(5);
pub const SNEQ_2: LTerm = LTerm::make_atom(6);
pub const SEQ_2: LTerm = LTerm::make_atom(7);
pub const SLE_2: LTerm = LTerm::make_atom(8);
pub const SYM_EQ_EQ: LTerm = LTerm::make_atom(9);
pub const SGT_2: LTerm = LTerm::make_atom(10);
pub const SGE_2: LTerm = LTerm::make_atom(11);
pub const APPLY: LTerm = LTerm::make_atom(12);
pub const BADARG: LTerm = LTerm::make_atom(13);
pub const BADARITH: LTerm = LTerm::make_atom(14);
pub const BADARITY: LTerm = LTerm::make_atom(15);
pub const BADFUN: LTerm = LTerm::make_atom(16);
pub const BADMATCH: LTerm = LTerm::make_atom(17);
pub const CASE_CLAUSE: LTerm = LTerm::make_atom(18);
pub const ERLANG: LTerm = LTerm::make_atom(19);
pub const ERROR: LTerm = LTerm::make_atom(20);
pub const EXIT: LTerm = LTerm::make_atom(21);
pub const FALSE: LTerm = LTerm::make_atom(22);
pub const FUNCTION_CLAUSE: LTerm = LTerm::make_atom(23);
pub const HIGH: LTerm = LTerm::make_atom(24);
pub const IF_CLAUSE: LTerm = LTerm::make_atom(25);
pub const INIT: LTerm = LTerm::make_atom(26);
pub const KILL: LTerm = LTerm::make_atom(27);
pub const KILLED: LTerm = LTerm::make_atom(28);
pub const LENGTH: LTerm = LTerm::make_atom(29);
pub const LOW: LTerm = LTerm::make_atom(30);
pub const MAKE_FUN: LTerm = LTerm::make_atom(31);
pub const NIF_ERROR: LTerm = LTerm::make_atom(32);
pub const NOCATCH: LTerm = LTerm::make_atom(33);
pub const NORMAL: LTerm = LTerm::make_atom(34);
pub const OK: LTerm = LTerm::make_atom(35);
pub const SELF: LTerm = LTerm::make_atom(36);
pub const SPAWN: LTerm = LTerm::make_atom(37);
pub const SYSTEM_LIMIT: LTerm = LTerm::make_atom(38);
pub const THROW: LTerm = LTerm::make_atom(39);
pub const TRAP_EXIT: LTerm = LTerm::make_atom(40);
pub const TRUE: LTerm = LTerm::make_atom(41);
pub const UNDEF: LTerm = LTerm::make_atom(42);
pub const UNDEFINED: LTerm = LTerm::make_atom(43);

pub static ATOM_INIT_NAMES: &'static [&'static str] = &[
  "*", // id=0
  "+", // id=1
  "++", // id=2
  "-", // id=3
  "/=", // id=4
  "<", // id=5
  "=/=", // id=6
  "=:=", // id=7
  "=<", // id=8
  "==", // id=9
  ">", // id=10
  ">=", // id=11
  "apply", // id=12
  "badarg", // id=13
  "badarith", // id=14
  "badarity", // id=15
  "badfun", // id=16
  "badmatch", // id=17
  "case_clause", // id=18
  "erlang", // id=19
  "error", // id=20
  "exit", // id=21
  "false", // id=22
  "function_clause", // id=23
  "high", // id=24
  "if_clause", // id=25
  "init", // id=26
  "kill", // id=27
  "killed", // id=28
  "length", // id=29
  "low", // id=30
  "make_fun", // id=31
  "nif_error", // id=32
  "nocatch", // id=33
  "normal", // id=34
  "ok", // id=35
  "self", // id=36
  "spawn", // id=37
  "system_limit", // id=38
  "throw", // id=39
  "trap_exit", // id=40
  "true", // id=41
  "undef", // id=42
  "undefined", // id=43
];
