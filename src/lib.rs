//! A module for modeling the behavior of std functions using integer
//! arithmetic.
//!
//! Currently, only integer intervals are supported, but floating point
//! intervals can be added later.

mod arithmetic;
mod bits;
mod boolean;
mod iinterval;

pub use arithmetic::*;
pub use boolean::*;
pub use iinterval::*;
