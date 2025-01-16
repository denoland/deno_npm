#[cfg(feature = "sync")]
mod internal {
  use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

  #[derive(Debug, Default)]
  pub struct MaybeRefCell<T> {
    inner: Arc<RwLock<T>>,
  }

  impl<T> MaybeRefCell<T> {
    pub fn new(value: T) -> Self {
      Self {
        inner: Arc::new(RwLock::new(value)),
      }
    }

    pub fn borrow(&self) -> RwLockReadGuard<'_, T> {
      self.inner.read().unwrap()
    }

    pub fn borrow_mut(&self) -> RwLockWriteGuard<'_, T> {
      self.inner.write().unwrap()
    }
  }

  impl<T> Clone for MaybeRefCell<T> {
    fn clone(&self) -> Self {
      Self {
        inner: Arc::clone(&self.inner),
      }
    }
  }

  pub use std::sync::Arc as MaybeArc;
}

#[cfg(not(feature = "sync"))]
mod internal {
  pub use std::cell::RefCell as MaybeRefCell;
  pub use std::rc::Rc as MaybeArc;
}

pub use internal::{MaybeArc, MaybeRefCell};
