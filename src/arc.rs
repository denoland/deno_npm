#[cfg(feature = "sync")]
pub use std::sync::Arc as MaybeArc;
#[cfg(feature = "sync")]
pub use std::sync::RwLock as MaybeRefCell;

#[cfg(not(feature = "sync"))]
#[derive(Default, Debug)]
pub struct ThreadSafeRefCell<T> {
  inner: std::sync::Arc<std::sync::Mutex<T>>,
}

impl<T> ThreadSafeRefCell<T> {
  pub fn new(value: T) -> Self {
    ThreadSafeRefCell {
      inner: std::sync::Arc::new(std::sync::Mutex::new(value)),
    }
  }

  pub fn borrow(&self) -> std::sync::MutexGuard<'_, T> {
    self.inner.lock().unwrap()
  }

  pub fn borrow_mut(&self) -> std::sync::MutexGuard<'_, T> {
    self.inner.lock().unwrap()
  }
}

impl<T> Clone for ThreadSafeRefCell<T> {
  fn clone(&self) -> Self {
    ThreadSafeRefCell {
      inner: std::sync::Arc::clone(&self.inner),
    }
  }
}
pub use ThreadSafeRefCell as MaybeRefCell;

#[cfg(not(feature = "sync"))]
pub use std::rc::Rc as MaybeArc;
