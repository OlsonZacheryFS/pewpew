use super::{
    error::{CreateExprError, EvalExprError, EvalExprErrorInner, IntoStreamError},
    templating::{Segment, TemplateType, True, OK},
};
use boa_engine::{
    object::{JsFunction, ObjectInitializer},
    prelude::*,
    property::Attribute,
};
use derivative::Derivative;
use diplomatic_bag::DiplomaticBag;
use futures::{Stream, TryStreamExt};
use itertools::Itertools;
use std::{
    borrow::Cow,
    cell::{OnceCell, RefCell},
    collections::{BTreeMap, BTreeSet},
    error::Error as StdError,
};
use zip_all::zip_all_map;

pub type ProviderStreamStream<Ar, E> =
    Box<dyn Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + Unpin + 'static>;

pub trait ProviderStream<Ar: Clone + Send + Unpin + 'static> {
    type Err: Unpin + std::error::Error;

    fn as_stream(&self) -> ProviderStreamStream<Ar, Self::Err>;
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct EvalExpr {
    #[derivative(Debug = "ignore")]
    ctx: DiplomaticBag<(RefCell<Context>, JsFunction)>,
    needed: Vec<String>,
}

impl EvalExpr {
    pub fn from_template<T>(script: Vec<Segment<T, True>>) -> Result<Self, CreateExprError>
    where
        T: TemplateType,
        T::ProvAllowed: OK,
    {
        let mut needed = Vec::new();
        let uses_p = OnceCell::new();
        let script = format!(
            "function ____eval(____provider_values){{ return {}; }}",
            script
                .into_iter()
                .map(|s| match s {
                    Segment::Raw(s) => s,
                    Segment::Prov(p, ..) => {
                        let s = format!("____provider_values.{p}");
                        let _ = uses_p.set(true);
                        needed.push(p);
                        s
                    }
                    _ => unreachable!(),
                })
                .collect::<String>()
        );
        if !uses_p.into_inner().unwrap_or(false) {
            // TODO: warn that a script with no provider reads was used with logging.
            eprintln!(
                "this script doesn't read from any providers; consider a literal or vars template"
            );
        }
        Ok(Self {
            ctx: DiplomaticBag::<Result<_, CreateExprError>>::new(move |_| {
                let mut ctx = builtins::get_default_context();
                ctx.eval(script).map_err(CreateExprError::fn_err)?;
                let efn = ctx
                    .eval("____eval")
                    .ok()
                    .and_then(|v| v.as_object().cloned())
                    .and_then(JsFunction::from_object)
                    .expect("just created eval fn; should be fine");
                Ok((RefCell::new(ctx), efn))
            })
            .transpose()
            .map_err(DiplomaticBag::into_inner)?,
            needed,
        })
    }

    pub fn required_providers(&self) -> BTreeSet<&str> {
        self.needed.iter().map(|s| s.as_str()).collect()
    }

    pub fn evaluate(&self, data: Cow<'_, serde_json::Value>) -> Result<String, EvalExprError> {
        let values = data
            .as_object()
            .ok_or_else(|| EvalExprError("provided data was not a Map".to_owned()))?
            .into_iter()
            .map(|(s, v)| (s.clone(), (v.clone(), vec![])))
            .collect();
        self.ctx.as_ref().and_then(move |_, (ctx, efn)| {
            let ctx = &mut *ctx.borrow_mut();
            Ok(Self::eval_raw::<()>(ctx, efn, values)?.0.to_string())
        })
    }

    fn eval_raw<Ar>(
        ctx: &mut Context,
        efn: &JsFunction,
        values: BTreeMap<String, (serde_json::Value, Vec<Ar>)>,
    ) -> Result<(serde_json::Value, Vec<Ar>), EvalExprErrorInner> {
        let values: BTreeMap<_, _> = values
            .into_iter()
            .map(|(n, (v, ar))| {
                JsValue::from_json(&v, ctx)
                    .map_err(EvalExprErrorInner::InvalidJsonFromProvider)
                    .map(|v| (n, (v, ar)))
            })
            .collect::<Result<_, _>>()?;
        let mut object = ObjectInitializer::new(ctx);
        for (name, (value, _)) in values.iter() {
            object.property(name.as_str(), value, Attribute::READONLY);
        }
        let object = object.build();
        Ok((
            efn.call(&JsValue::Null, &[object.into()], ctx)
                .map_err(EvalExprErrorInner::ExecutionError)?
                .to_json(ctx)
                .map_err(EvalExprErrorInner::InvalidResultJson)?,
            values.into_iter().flat_map(|v| v.1 .1).collect_vec(),
        ))
    }

    pub fn into_stream<P, Ar, E>(
        self,
        providers: &BTreeMap<String, P>,
    ) -> Result<
        impl Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + 'static,
        IntoStreamError,
    >
    where
        P: ProviderStream<Ar, Err = E> + Sized + 'static,
        Ar: Clone + Send + Unpin + 'static,
        E: Send + Unpin + StdError + 'static + From<EvalExprError>,
    {
        let Self { ctx, needed } = self;
        let providers = needed
            .into_iter()
            .map(|pn| {
                providers
                    .get(&pn)
                    .map(|p| (pn.clone(), p.as_stream()))
                    .ok_or_else(|| IntoStreamError::MissingProvider(pn.clone()))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;
        Ok(zip_all_map(providers, true).and_then(move |values| {
            ctx.as_ref().and_then(|_, (ctx, efn)| {
                use futures::future::{err, ok};
                match Self::eval_raw(&mut ctx.borrow_mut(), efn, values) {
                    Ok(v) => ok(v),
                    Err(e) => err(EvalExprError::from(e).into()),
                }
            })
        }))
    }
}

#[cfg(test)]
mod tests {
    use boa_engine::{Context, JsValue};

    #[test]
    fn test_default_context() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(ctx.eval(r#"parseInt("5")"#), Ok(JsValue::Integer(5)));
        assert_eq!(ctx.eval(r#"parseFloat("5.1")"#), Ok(JsValue::Rational(5.1)));
    }
}

pub use builtins::get_default_context;

#[scripting_macros::boa_mod]
mod builtins {
    use scripting_macros::boa_fn;
    use std::str::FromStr;

    #[boa_fn(jsname = "parseInt")]
    pub fn parse_int(s: &str) -> Result<i64, <i64 as FromStr>::Err> {
        s.parse()
    }

    #[boa_fn(jsname = "parseFloat")]
    pub fn parse_float(s: &str) -> Option<f64> {
        s.parse().ok()
    }

    mod helper {
        use boa_engine::{Context, JsResult, JsValue};
        use std::fmt::Display;

        pub(super) trait GetAs<T> {
            fn get_as(self, ctx: &mut Context) -> JsResult<T>;
        }

        impl<'a> GetAs<&'a str> for &'a JsValue {
            fn get_as(self, ctx: &mut Context) -> JsResult<&'a str> {
                Ok(self
                    .as_string()
                    .ok_or_else(|| ctx.construct_type_error("not a string"))?
                    .as_str())
            }
        }

        pub(super) trait AsJsResult {
            fn as_js_result(self) -> JsResult<JsValue>;
        }

        impl<T> AsJsResult for Option<T>
        where
            JsValue: From<T>,
        {
            fn as_js_result(self) -> JsResult<JsValue> {
                self.map(JsValue::from)
                    .ok_or_else(|| JsValue::String("missing value".into()))
            }
        }

        impl<T, E> AsJsResult for Result<T, E>
        where
            JsValue: From<T>,
            E: Display,
        {
            fn as_js_result(self) -> JsResult<JsValue> {
                self.map(JsValue::from)
                    .map_err(|e| JsValue::String(e.to_string().into()))
            }
        }
    }
}
