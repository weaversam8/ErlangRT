use crate::term::value::Term;

impl Term {
  /// Create a NON_VALUE.
  #[inline]
  pub fn non_value() -> Self {
    Self::make_boxed(core::ptr::null::<usize>())
  }

  /// Check whether a value is a NON_VALUE.
  #[inline]
  pub fn is_non_value(self) -> bool {
    // Inlining is_boxed and get_box_ptr + is null should be optimized nicely
    // into `cmp rdi, PrimaryTag::BOXED_PTR` or something like that
    self.is_boxed() && self.get_box_ptr::<usize>().is_null()
  }

  /// Check whether a value is NOT a NON_VALUE.
  #[inline]
  pub fn is_value(self) -> bool {
    !self.is_non_value()
  }
}