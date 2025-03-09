#![no_std]

#[cfg(feature = "compiler")]
#[macro_use]
extern crate std;


#[cfg(feature = "compiler")]
pub mod compiler;

#[doc(hidden)]
pub mod __{
    pub use core;
}