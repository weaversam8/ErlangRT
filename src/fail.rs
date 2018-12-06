//!
//! Generic errors used everywhere in the VM runtime.
//!
use beam::compact_term::CTError;
use emulator::heap::HeapError;
use rt_util::bin_reader;

use rt_defs::ExceptionType;
use rt_util::bin_reader::ReadError;
use std::convert::From;
use term::lterm::LTerm;


// TODO: Rename to RTError-something
#[derive(Debug)]
pub enum Error {
  FileNotFound(String),
  ETFParseError(String),
  ReadError(ReadError),

  //--- Code loading ---
  CodeLoadingFailed(String),
  CodeLoadingCompactTerm(CTError),
  //PrematureEOF,

  //--- Code server, lookups ---
  ModuleNotFound(String),
  FunctionNotFound(String),
  BifNotFound(String),

  //--- Memory allocation events and errors ---
  AtomNotExist(String),
  HeapError(HeapError),
  //StackIndexRange,

  //--- VM Checks --
  Exception(ExceptionType, LTerm), // type, value
  TermIsNotABoxed,
  BoxedIsNotAClosure,
  BoxedIsNotAnImport,
  BoxedIsNotAnExport,
  BoxedIsNotATuple,

  //--- Binary ---
  CannotCopyIntoRefbin,
  HeapBinTooSmall(usize, usize), // want bytes, have bytes
  ProcBinTooSmall(usize, usize), // want bytes, have bytes
}


impl From<bin_reader::ReadError> for Error {
  fn from(e: bin_reader::ReadError) -> Self {
    Error::ReadError(e)
  }
}

impl From<HeapError> for Error {
  fn from(e: HeapError) -> Self {
    Error::HeapError(e)
  }
}


/// A templated error type based on `fail::Error`.
// TODO: Rename to RTResult-something
pub type Hopefully<T> = Result<T, Error>;

//impl fmt::Debug for Error {
//  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//    match self {
//      &Error::FileNotFound(ref filename) =>
//        return write!(f, "File not found: {}", filename)
//    }
//    write!(f, "Some internal error")
//  }
//}
