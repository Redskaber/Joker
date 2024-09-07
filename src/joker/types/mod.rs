mod env;
mod inferrer;
mod this;
mod type_;

// pub use env::TypeEnv;
pub use inferrer::TypeInferrer;
pub use this::{DeepClone, FromObject, Object};
pub use type_::{FromParamPair, IsInstance, ParamPair, Type};
