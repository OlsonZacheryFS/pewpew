//pub mod configv1;
//pub use configv1::*;

mod configv2;
pub use configv2::*;

mod shared;

pub use shared::duration_from_string;
