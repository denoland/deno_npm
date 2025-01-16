#[cfg(feature = "sync")]
pub use std::sync::Arc as MaybeArc;

#[cfg(not(feature = "sync"))]
pub use std::rc::Rc as MaybeArc;
