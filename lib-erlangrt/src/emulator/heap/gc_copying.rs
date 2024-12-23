//! Copying Garbage Collector
//! A simple implementation of half-heap copying (evicting) garbage collector
//! somewhat similar to what is used in Erlang/OTP.
use log::debug;

use crate::{
  emulator::heap::{gc_trait::TGc, heap_trait::THeap, *},
  fail::RtResult,
  term::{heap_walker::*, Term},
};

pub struct CopyingGc {}

impl TGc for CopyingGc {
  fn new() -> Self {
    Self {}
  }

  fn garbage_collect(
    _heap: &dyn THeap,
    mut walker: HeapWalker,
    mut roots: Box<dyn TRootIterator>,
  ) -> RtResult<()> {
    debug!("Copying GC");

    roots.roots_begin();
    loop {
      let r = roots.roots_next();
      if r.is_null() {
        break;
      }
      debug!("root: {:?}", unsafe { *r });
    }

    loop {
      let p = walker.next();
      if p.is_null() {
        break;
      }
      let pval = unsafe { Term::from_raw(*p) };
      debug!("Heapwalker: {pval}");
    }

    unimplemented!("Copying GC: Done")
  }
}
