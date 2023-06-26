use boa_engine::JsValue;
use std::error::Error as SError;
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
        error: Box<dyn SError>,
    },
    #[error("{0}")]
    CreateExpr(#[from] CreateExprError),
}

#[derive(Debug, Error)]
pub enum CreateExprError {
    #[error("failure building JS function: {0}")]
    BuildFnFailure(String),
}

impl CreateExprError {
    // JsValue is not `Send`, so it is reported as a String first
    pub(crate) fn fn_err(js: JsValue) -> Self {
        Self::BuildFnFailure(js.display().to_string())
    }
}

#[derive(Debug, Error)]
pub enum IntoStreamError {
    #[error("missing provider: {0}")]
    MissingProvider(String),
}

#[derive(Debug, Error, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[error("{0}")]
pub struct EvalExprError(pub(crate) String);

impl From<EvalExprErrorInner> for EvalExprError {
    // JsValue is not `Send`, so it is reported as a String first
    fn from(value: EvalExprErrorInner) -> Self {
        Self(value.to_string())
    }
}

#[derive(Debug, Error)]
pub(crate) enum EvalExprErrorInner {
    #[error("provider returned invalid json: {}", .0.display())]
    InvalidJsonFromProvider(JsValue),
    #[error("error executing JS code: {}", .0.display())]
    ExecutionError(JsValue),
    #[error("expression returned invalid json: {}", .0.display())]
    InvalidResultJson(JsValue),
}
