#[cfg(feature = "sync")]
pub use std::sync::Arc as MaybeArc;
#[cfg(feature = "sync")]
pub use std::sync::RwLock as MaybeRwLock;

#[cfg(not(feature = "sync"))]
pub use std::cell::RefCell as MaybeRwLock;
#[cfg(not(feature = "sync"))]
pub use std::rc::Rc as MaybeArc;
