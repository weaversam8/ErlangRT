//! Classic Heap, similar to the existing heap and GC in Erlang/OTP.
//!
//! * Incremental allocation.
//! * Second heap is created on GC. Live data is moved into the new heap.
//!   The old heap is discarded.
//!
//! Possible improvements:
//!
//! * The young values get garbaged more often, introduce an age mark.
use crate::{
  defs::{Word, SizeWords},
  emulator::heap::{catch::NextCatchResult, gc_trait::TGc, heap_trait::*, iter, *},
  fail::{RtErr, RtResult},
  term::{heap_walker::*, Term},
};
use colored::Colorize;
use log::{debug, trace};
use core::fmt;
use gc_arena::{
  lock::RefLock, Arena, Collect, Gc as ArenaGc, Mutation, Rootable
};

/// Default heap size for constants (literals) when loading a module.
const DEFAULT_LIT_HEAP: usize = 8192;

/// Default heap size when spawning a process. (default: 300)
const DEFAULT_PROC_HEAP: usize = 1024;
const BINARY_HEAP_CAPACITY: usize = 65536; // 64k*8 = 512kb

#[derive(Collect)]
#[collect(no_drop)]
struct HeapData {
  data: Vec<Word>,
  /// Heap top, begins at 0 and grows up towards the `stack_top`.
  heap_top: usize,
  /// Stack top, begins at the end of capacity and grows down.
  stack_top: usize,
  /// Marks end of the stack and also end of the heap.
  capacity: usize,
}

type HeapDataRef<'gc> = ArenaGc<'gc, RefLock<HeapData>>;

/// A heap structure which allocates incrementally forward.
/// Stack grows backwards until they meet with the heap.
/// When stack meets heap, a new heap is created and GC moves live data there.
/// The old heap is discarded.
pub struct IncrementalHeap<GC>
where
  GC: TGc,
{
  gc: GC,
  arena: Arena<Rootable![HeapDataRef<'_>]>,
}

impl<'gc, GC: TGc> fmt::Debug for IncrementalHeap<GC> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "IncrementalHeap{{ cap: {}, used: {} }}",
      self.get_heap_max_capacity(),
      self.get_heap_used_words()
    )
  }
}

impl<'gc, GC: TGc> THeap for IncrementalHeap<GC> {
  fn alloc(&mut self, n: SizeWords, fill: AllocInit) -> RtResult<*mut Word> {
    self.arena.mutate(|mc, heap| {
      let n_words = n.words;

      {
        let heap = heap.borrow();
        let pos = heap.heap_top;

        // Explicitly forbid expanding without a GC, fail if capacity is exceeded
        // This situation has to be detected before we arrive here
        if pos + n_words >= heap.stack_top {
          panic!("Heap is full requested={}", n);
        }
      }

      // Assume we can grow the data without reallocating
      let new_chunk = unsafe { get_heap_start_ptr_mut(mc, heap).add(heap.borrow().heap_top) };

      if fill == AllocInit::Nil {
        let raw_nil = Term::nil().raw();
        unsafe {
          for i in 0..n_words {
            new_chunk.add(i).write(raw_nil)
          }
        }
      }

      {
        let mut heap = heap.borrow_mut(mc);
        heap.heap_top += n_words;
      }

      Ok(new_chunk)
    })

  }

  #[inline]
  fn garbage_collect(&mut self, roots: Box<dyn TRootIterator>) -> RtResult<()> {
    // TODO: garbage collection is still occurring outside of arena-gc, this
    // should defer to arena-gc for the actual collection.
    self.arena.mutate(|mc, heap| {
      let walker = HeapWalker::new(get_heap_start_ptr_mut(mc, heap), get_heap_top_ptr_mut_impl(mc, heap));
      GC::garbage_collect(self, walker, roots)
    })
  }

  fn get_y(&self, index: Word) -> RtResult<Term> {
    self.arena.mutate(|_, heap| {
      if !self.stack_have_y(index) {
        debug!(
          "Stack value requested y{}, depth={}",
          index,
          stack_depth_impl(heap)
        );
        return Err(RtErr::StackIndexRange(index));
      }
    
      let heap = heap.borrow();
      let pos = index + heap.stack_top + 1;
      let result = Term::from_raw(heap.data[pos]);
      debug_assert!(result.is_value(), "Should never get a #Nonvalue<> from y[]");
      Ok(result)
    })
  }

  #[inline]
  fn get_y_unchecked(&self, index: Word) -> Term {
    self.arena.mutate(|_, heap| {
      let heap = heap.borrow();
      let pos = index + heap.stack_top + 1;
      Term::from_raw(heap.data[pos])
    })
  }

  /// Set stack value (`index`th from stack top) to `val`.
  fn set_y(&mut self, index: Word, val: Term) -> RtResult<()> {
    self.arena.mutate(|mc, heap| {
      let mut heap = heap.borrow_mut(mc);
      debug_assert!(val.is_value(), "Should never set y[] to a #Nonvalue<>");
      if !self.stack_have_y(index) {
        return Err(RtErr::StackIndexRange(index));
      }
      if cfg!(feature = "trace_stack_changes") {
        trace!("{}{} = {}", "set y".green(), index, val);
      }
      let new_pos = index + heap.stack_top + 1;
      heap.data[new_pos] = val.raw();
      Ok(())
    })
  }

  /// Take `cp` from stack top and deallocate `n+1` words of stack.
  fn stack_deallocate(&mut self, n: usize) -> Term {
    self.arena.mutate(|mc, heap| {
      let mut heap = heap.borrow_mut(mc);
      assert!(
        heap.stack_top + n < heap.capacity,
        "Failed to dealloc {}+1 words (s_top {}, s_end {})",
        n,
        heap.stack_top,
        heap.capacity
      );

      let cp = Term::from_raw(heap.data[heap.stack_top]);
      assert!(
        cp.is_cp(),
        "Dealloc expected a CP value on stack top, got {}",
        cp
      );
      heap.stack_top += n + 1;
        
      cp
    })
  }

  //  /// Express the intent to allocate `size` words on the heap, which may either
  //  /// include an attempt to GC, or incur a heap fragment allocation.
  //  /// Does not immediately allocate.
  //  fn allocate_intent(&mut self, size: WordSize, _live: usize) -> RtResult<()> {
  //    if self.heap_check_available(size) {
  //      return Ok(());
  //    }
  //    Err(RtErr::HeapIsFull)
  //  }

  //  fn allocate_intent_no_gc(&mut self, _size: WordSize) -> RtResult<()> {
  //    Ok(())
  //  }

  #[inline]
  fn heap_check_available(&self, need: SizeWords) -> bool {
    self.arena.mutate(|_, heap| heap_check_available_impl(heap, need))
  }

  #[inline]
  fn stack_check_available(&self, need: SizeWords) -> bool {
    self.arena.mutate(|_, heap| stack_check_available_impl(heap, need))
  }

  /// Allocate stack cells without checking. Call `stack_have(n)` beforehand.
  fn stack_alloc(&mut self, need: SizeWords, _extra: SizeWords, fill: AllocInit) {
    self.arena.mutate(|mc, heap| {
      {
        let mut heap = heap.borrow_mut(mc);
        if need.words == 0 {
          return;
        }
        heap.stack_top -= need.words;
      }

      // Clear the new cells
      let raw_nil = Term::nil().raw();
      unsafe {
        let p = get_heap_start_ptr_mut(mc, heap).add(heap.borrow().stack_top);

        if fill == AllocInit::Nil {
          for y in 0..need.words {
            p.add(y).write(raw_nil)
          }
        }
      }
    });
  }

  fn stack_depth(&self) -> usize {
    self.arena.mutate(|_, heap| stack_depth_impl(heap))
  }

  /// Push a Term to stack without checking. Call `stack_have(1)` beforehand.
  #[inline]
  fn stack_push_lterm_unchecked(&mut self, val: Term) {
    if cfg!(feature = "trace_stack_changes") {
      trace!("{} {}", "push (unchecked)".green(), val);
    }
    self.arena.mutate(|mc, heap| {
      let mut heap = heap.borrow_mut(mc);
      heap.stack_top -= 1;
      let pos = heap.stack_top;
      heap.data[pos] = val.raw();
    });
  }

  /// Sets the stack top.
  /// Arg: new_stack_top - offset from the heap end
  fn drop_stack_words(&mut self, n_drop: usize) {
    self.arena.mutate(|mc, heap| {
      debug!("drop_stack_words {n_drop}");
      let mut heap = heap.borrow_mut(mc);
      assert!(heap.stack_top + n_drop < heap.capacity);
      heap.stack_top += n_drop;
    });
  }

  /// Go through stack values searching for a stored CP, skip if it does not
  /// point to a catch instruction.
  /// Returns: the location stored on stack
  unsafe fn unroll_stack_until_catch(&self) -> Option<NextCatchResult> {
    self.arena.mutate(|_, heap| {

      let mut ptr: *const Word = get_stack_top_ptr(heap);
      let stack_start: *const Word = get_stack_start_ptr(heap);
      // Counter how many stack cells to drop
      let mut stack_drop = 0usize;

      loop {
        if ptr >= stack_start {
          return None;
        }
        // Hope we found a CP on stack (good!)
        let term_at_ptr = Term::from_raw(ptr.read());

        if term_at_ptr.is_catch() {
          // Typical stack frame looks like:
          // >>-top-> CP Catch ...
          // Drop 1 less word than where the catch was found to preserve that CP
          return Some(NextCatchResult {
            loc: term_at_ptr.get_catch_ptr(),
            stack_drop: stack_drop - 1,
          });
        }
        ptr = ptr.add(1);
        stack_drop += 1;
      }
    })
  }

  /// Create a constant iterator for walking the heap.
  /// This is used by heap walkers such as "dump.rs"
  unsafe fn heap_iter(&self) -> iter::HeapIterator {
    self.arena.mutate(|_, heap| {
      let begin = get_heap_start_ptr(heap) as *const Term;
      let last = heap.borrow().heap_top as isize;
      iter::HeapIterator::new(begin, begin.offset(last))
    })
  }

  fn belongs_to_heap(&self, p: *const Word) -> bool {
    self.arena.mutate(|_, heap| {
      p < get_heap_start_ptr(heap) || p >= get_heap_top_ptr_impl(heap)
  })
  }

  fn stack_dump(&self) {
    if self.stack_depth() == 0 {
      debug!("stack: empty");
      return;
    }

    let mut i = 0;
    let max_i = self.stack_depth() - 1;
    loop {
      if i >= max_i || i >= 10 {
        break;
      }
      debug!("stack Y[{}] = {}", i, self.get_y_unchecked(i));
      i += 1;
    }
  }
}

// === === ===

impl<'gc, GC: TGc> IncrementalHeap<GC> {
  fn get_size_for(d: Designation) -> usize {
    match d {
      Designation::ProcessHeap => DEFAULT_PROC_HEAP,
      Designation::ModuleLiterals => DEFAULT_LIT_HEAP,
      Designation::BinaryHeap => BINARY_HEAP_CAPACITY,
      Designation::TransientDestructible => 1,
      Designation::ProgramArgumentsHeap => 512,
    }
  }

  pub fn new(designation: Designation) -> Self {
    let capacity = Self::get_size_for(designation);
    assert!(capacity > 0);

    let arena = Arena::<Rootable![HeapDataRef<'_>]>::new(|mc| {
      ArenaGc::new(mc, RefLock::new(HeapData {
        data: Vec::with_capacity(capacity),
        heap_top: 0,
        stack_top: capacity,
        capacity,
      }))
    });

    let h = Self {
      gc: GC::new(),
      arena
    };
    
    h.arena.mutate(|mc, heap| {
      // this is safe since the data vector has not been touched yet
      unsafe { heap.borrow_mut(mc).data.set_len(capacity) }
    });

    h
  }

  /// How many words do we have before it will require GC/growth.
  #[inline]
  fn get_heap_max_capacity(&self) -> usize {
    // "Mutate" here refers to anything that is not GC, so we're not actually
    // changing the data here.
    self.arena.mutate(|_, heap| heap.borrow().data.capacity())
  }

  /// Heap usage stat.
  #[inline]
  fn get_heap_used_words(&self) -> usize {
    self.arena.mutate(|_, heap| heap.borrow().heap_top)
  }

  #[allow(dead_code)]
  #[inline]
  fn get_heap_available(&self) -> usize {
    self.arena.mutate(|_, heap| {
      let heap = heap.borrow();
      debug_assert!(heap.stack_top > heap.heap_top);
      heap.stack_top - heap.heap_top
    })
  }

  /// Get pointer to end of the allocated heap (below the stack top).
  #[inline]
  pub fn get_heap_top_ptr(&self) -> *const Word {
    self.arena.mutate(|_, heap| get_heap_top_ptr_impl(heap))
  }

  /// Get pointer to end of the allocated heap (below the stack top).
  #[inline]
  pub fn get_heap_top_ptr_mut(&mut self) -> *mut Word {
    self.arena.mutate(|mc, heap| get_heap_top_ptr_mut_impl(mc, heap))
  }

  #[allow(dead_code)]
  pub fn stack_info(&self) {
    self.arena.mutate(|_, data| {
      let data = data.borrow();
      debug!("Stack (s_top {}, s_end {})", data.stack_top, data.capacity)
    });
  }

  /// Check whether `y+1`-th element can be found in stack
  #[inline]
  pub fn stack_have_y(&self, y: Word) -> bool {
    self.arena.mutate(|_, data| {
      let data = data.borrow();
      data.capacity - data.stack_top >= y + 1
    })
  }
}

fn stack_depth_impl<'gc>(heap: &HeapDataRef<'gc>) -> usize {
  let heap = heap.borrow();
  heap.capacity - heap.stack_top
}

#[inline]
fn heap_check_available_impl<'gc>(heap: &HeapDataRef<'gc>, need: SizeWords) -> bool {
  let heap = heap.borrow();
  heap.heap_top + need.words <= heap.stack_top
}

#[inline]
fn stack_check_available_impl<'gc>(heap: &HeapDataRef<'gc>, need: SizeWords) -> bool {
  let heap = heap.borrow();
  heap.heap_top + need.words <= heap.stack_top
}
  
fn get_heap_top_ptr_impl<'gc>(heap: &HeapDataRef<'gc>) -> *const Word {
  let ptr = get_heap_start_ptr(heap);
  unsafe { ptr.add(heap.borrow().heap_top) }
}

#[inline]
pub fn get_heap_start_ptr<'gc>(heap: &HeapDataRef<'gc>) -> *const Word {
  heap.borrow().data.as_ptr()
}

#[inline]
fn get_heap_start_ptr_mut<'gc>(mc: &Mutation<'gc>, heap: &HeapDataRef<'gc>) -> *mut Word {
  heap.borrow_mut(mc).data.as_mut_ptr()
}

fn get_heap_top_ptr_mut_impl<'gc>(mc: &Mutation<'gc>, heap: &HeapDataRef<'gc>) -> *mut Word {
  let ptr = get_heap_start_ptr_mut(mc, heap);
  unsafe { ptr.add(heap.borrow().heap_top) }
}

/// Stack start is same as end of everything, pointer to the first word after
/// the allocated memory, used as limit when iterating the stack.
#[inline]
unsafe fn get_stack_start_ptr<'gc>(heap: &HeapDataRef<'gc>) -> *const Word {
  get_end_ptr(heap)
}

#[inline]
unsafe fn get_end_ptr<'gc>(heap: &HeapDataRef<'gc>) -> *const Word {
  let ptr = get_heap_start_ptr(heap);
  ptr.add(heap.borrow().capacity)
}

#[inline]
unsafe fn get_stack_top_ptr<'gc>(heap: &HeapDataRef<'gc>) -> *const Word {
  let ptr = get_heap_start_ptr(heap);
  ptr.add(heap.borrow().stack_top)
}