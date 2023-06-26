use boa_engine::JsValue;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LoadTestGenError {
    #[error("error parsing yaml: {0}")]
    YamlParse(#[from] serde_yaml::Error),
    #[error("{0}")]
    MissingEnvVar(#[from] MissingEnvVar),
    #[error("error inserting static vars: {0}")]
    VarsError(#[from] VarsError),
}

#[derive(Debug, Error)]
#[error("missing environment variable {0}")]
pub struct MissingEnvVar(pub(crate) String);

#[derive(Debug, Error)]
pub enum VarsError {
    #[error("var at path \"{0}\" not found")]
    VarNotFound(String),
    #[error("resulting string \"{from}\", was not a valid {typename} ({error})")]
    InvalidString {
        typename: &'static str,
        from: String,
        #[source]
        error: Box<dyn std::error::Error>,
    },
}

#[derive(Debug, Error)]
pub enum CreateExprError {
    #[error("failure building JS function: {}", .0.display())]
    BuildFnFailure(JsValue),
}

#[derive(Debug, Error)]
pub enum IntoStreamError {
    #[error("missing provider: {0}")]
    MissingProvider(String),
}

#[derive(Debug, Error)]
pub(crate) enum EvalExprError {
    #[error("provider returned invalid json: {}", .0.display())]
    InvalidJsonFromProvider(JsValue),
}
