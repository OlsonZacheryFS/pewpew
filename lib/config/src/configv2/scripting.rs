use super::{
    error::{CreateExprError, EvalExprError, EvalExprErrorInner, IntoStreamError},
    templating::{False, Segment, TemplateType, True},
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
    cell::RefCell,
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

pub fn eval_direct(code: &str) -> Result<String, EvalExprError> {
    get_default_context()
        .eval(code)
        .map_err(EvalExprErrorInner::ExecutionError)
        .map_err(Into::into)
        .map(|js| js.display().to_string())
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
        T: TemplateType<ProvAllowed = True, EnvsAllowed = False>,
    {
        let mut needed = Vec::new();
        let script = format!(
            "function ____eval(____provider_values){{ return {}; }}",
            script
                .into_iter()
                .map(|s| match s {
                    Segment::Raw(s) => s,
                    Segment::Prov(p, ..) => {
                        let s = format!("____provider_values.{p}");
                        needed.push(p);
                        s
                    }
                    Segment::Env(_, no) => no.no(),
                    _ => unreachable!("should have inserted vars first"),
                })
                .collect::<String>()
        );
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
    use boa_engine::{object::JsArray, Context, JsValue};

    #[test]
    fn parse_funcs() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(ctx.eval(r#"parseInt("5")"#), Ok(JsValue::Integer(5)));
        assert_eq!(ctx.eval(r#"parseFloat("5.1")"#), Ok(JsValue::Rational(5.1)));
        assert_eq!(ctx.eval(r#"parseFloat("e")"#), Ok(JsValue::Null));
    }

    #[test]
    fn reapeat_fn() {
        let mut ctx: Context = super::builtins::get_default_context();
        let rep_arr = ctx.eval(r#"repeat(3)"#).unwrap();
        let rep_arr = rep_arr.as_object().unwrap();
        assert!(rep_arr.is_array());
        let rep_arr = JsArray::from_object(rep_arr.clone(), &mut ctx).unwrap();
        for _ in 0..3 {
            assert_eq!(rep_arr.pop(&mut ctx).unwrap(), JsValue::Null);
        }
        assert_eq!(rep_arr.pop(&mut ctx).unwrap(), JsValue::Undefined);
    }

    #[test]
    fn pad_fns() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(
            ctx.eval(r#"end_pad("foo", 6, "bar")"#),
            Ok(JsValue::String("foobar".into()))
        );
        assert_eq!(
            ctx.eval(r#"end_pad("foo", 7, "bar")"#),
            Ok(JsValue::String("foobarb".into()))
        );
        assert_eq!(
            ctx.eval(r#"end_pad("foo", 1, "fsdajlkvshduva")"#),
            Ok(JsValue::String("foo".into()))
        );
        assert_eq!(
            ctx.eval(r#"end_pad("foo", 4, "")"#),
            Ok(JsValue::String("foo".into()))
        );

        assert_eq!(
            ctx.eval(r#"start_pad("foo", 6, "bar")"#),
            Ok(JsValue::String("barfoo".into()))
        );
        assert_eq!(
            ctx.eval(r#"start_pad("foo", 7, "bar")"#),
            Ok(JsValue::String("barbfoo".into()))
        );
        assert_eq!(
            ctx.eval(r#"start_pad("foo", 1, "fsdajlkvshduva")"#),
            Ok(JsValue::String("foo".into()))
        );
        assert_eq!(
            ctx.eval(r#"start_pad("foo", 4, "")"#),
            Ok(JsValue::String("foo".into()))
        );

        assert_eq!(
            ctx.eval(r#"encode("foo=bar", "percent-userinfo")"#),
            Ok(JsValue::String("foo%3Dbar".into()))
        );
    }

    #[test]
    fn epoch_fn() {
        let mut ctx: Context = super::builtins::get_default_context();

        // These tests will break in May of 2033, when the timestamp reaches 2000000000
        let ep = ctx
            .eval(r#"epoch("s")"#)
            .unwrap()
            .as_string()
            .unwrap()
            .as_str()
            .to_owned();
        assert_eq!(ep.len(), 10);
        assert_eq!(&ep[..1], "1");
        let ep = ctx
            .eval(r#"epoch("ms")"#)
            .unwrap()
            .as_string()
            .unwrap()
            .as_str()
            .to_owned();
        assert_eq!(ep.len(), 13);
        assert_eq!(&ep[..1], "1");
        let ep = ctx
            .eval(r#"epoch("mu")"#)
            .unwrap()
            .as_string()
            .unwrap()
            .as_str()
            .to_owned();
        assert_eq!(ep.len(), 16);
        assert_eq!(&ep[..1], "1");
        let ep = ctx
            .eval(r#"epoch("ns")"#)
            .unwrap()
            .as_string()
            .unwrap()
            .as_str()
            .to_owned();
        assert_eq!(ep.len(), 19);
        assert_eq!(&ep[..1], "1");
    }

    #[test]
    fn entries_fn() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(
            ctx.eval(r#"entries({"foo": "bar", "baz": 123})"#)
                .unwrap()
                .to_json(&mut ctx)
                .unwrap(),
            serde_json::json!([["foo", "bar"], ["baz", 123]])
        );
        assert_eq!(
            ctx.eval(r#"entries(["abc", "def"])"#)
                .unwrap()
                .to_json(&mut ctx)
                .unwrap(),
            serde_json::json!([[0, "abc"], [1, "def"]])
        );
        assert_eq!(
            ctx.eval(r#"entries("xyz")"#)
                .unwrap()
                .to_json(&mut ctx)
                .unwrap(),
            serde_json::json!([[0, "x"], [1, "y"], [2, "z"]])
        );
        assert_eq!(ctx.eval("entries(null)"), Ok(JsValue::Null));
    }

    #[test]
    fn random_fn() {
        let mut ctx: Context = super::builtins::get_default_context();
        // not testing value ranges, just int * int -> int
        assert!(matches!(
            ctx.eval(r#"random(1, 4)"#),
            Ok(JsValue::Integer(_))
        ));
        assert!(matches!(
            ctx.eval(r#"random(1.1, 4)"#),
            Ok(JsValue::Rational(_))
        ));
        assert!(matches!(
            ctx.eval(r#"random(1, 4.1)"#),
            Ok(JsValue::Rational(_))
        ));
        assert!(matches!(
            ctx.eval(r#"random(1.001, 4.09)"#),
            Ok(JsValue::Rational(_))
        ));
    }

    #[test]
    fn range_fn() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(
            ctx.eval("range(1, 10)").unwrap().to_json(&mut ctx).unwrap(),
            serde_json::json!([1, 2, 3, 4, 5, 6, 7, 8, 9])
        );
        assert_eq!(
            ctx.eval("range(10, 1)").unwrap().to_json(&mut ctx).unwrap(),
            serde_json::json!([10, 9, 8, 7, 6, 5, 4, 3, 2])
        );
    }

    #[test]
    fn replace_fn() {
        let mut ctx: Context = super::builtins::get_default_context();
        assert_eq!(
            ctx.eval(r#"replace("foo", {"foo": "baz", "zed": ["abc", 123, "fooo"]}, "bar")"#)
                .unwrap()
                .to_json(&mut ctx)
                .unwrap(),
            serde_json::json!({"bar": "baz", "zed": ["abc", 123, "baro"]})
        )
    }
}

pub use builtins::get_default_context;

#[scripting_macros::boa_mod]
mod builtins {
    //! Built-in expression functions
    //!
    //! These functions are callable by the JS Runtime used for pewpew expressions.
    //!
    //! For the function to be properly handled and callable:
    //!
    //! - Any input type `T` must implement `JsInput`.
    //! - Any output type `O` must implement `AsJsResult`.
    //! - The `#[boa_fn]` attribute macro must be applied. A `jsname` parameter
    //!   may also be provided to set the name that this function will be callable
    //!   from the JS runtime as; with no `jsname`, the default will be the
    //!   native Rust function name.
    //!
    //! IMPORTANT: Do **NOT** let these functions panic if at all possible

    use crate::shared::{encode::Encoding, Epoch};
    use helper::{AnyAsString, NumType, OrNull};
    use rand::{thread_rng, Rng};
    use scripting_macros::boa_fn;
    use serde_json::Value as SJV;
    use std::cmp::Ordering;
    // use std::str::FromStr;

    #[boa_fn]
    fn encode(s: AnyAsString, e: Encoding) -> String {
        e.encode_str(&s.get())
    }

    #[boa_fn]
    fn end_pad(s: AnyAsString, min_length: i64, pad_string: &str) -> String {
        use unicode_segmentation::UnicodeSegmentation;
        let mut s = s.get();
        let needed_chars = (min_length as usize).saturating_sub(s.len());

        let pad_chars: String = pad_string
            .graphemes(true)
            .cycle()
            .take(needed_chars)
            .collect();

        s.push_str(&pad_chars);
        s
    }

    #[boa_fn]
    fn entries(value: SJV) -> SJV {
        fn collect<K: Into<SJV>, V: Into<SJV>, I: IntoIterator<Item = (K, V)>>(iter: I) -> SJV {
            iter.into_iter()
                .map(|(k, v)| SJV::Array(vec![k.into(), v.into()]))
                .collect::<Vec<_>>()
                .into()
        }
        match value {
            SJV::Array(a) => collect(a.into_iter().enumerate()),
            SJV::Object(o) => collect(o),
            SJV::String(s) => collect(s.chars().enumerate().map(|(i, c)| (i, c.to_string()))),
            other => other,
        }
    }

    #[boa_fn]
    fn epoch(e: Epoch) -> String {
        e.get().to_string()
    }

    #[boa_fn(jsname = "parseInt")]
    fn parse_int(s: AnyAsString) -> OrNull<i64> {
        s.get().parse().ok().into()
    }

    #[boa_fn(jsname = "parseFloat")]
    fn parse_float(s: AnyAsString) -> OrNull<f64> {
        s.get().parse().ok().into()
    }

    #[boa_fn]
    fn random(min: NumType, max: NumType) -> NumType {
        match (min, max) {
            (NumType::Int(i), NumType::Int(j)) => NumType::Int(thread_rng().gen_range(i..j)),
            (i, j) => {
                let (i, j) = (i.as_float(), j.as_float());
                NumType::Real(thread_rng().gen_range(i..j))
            }
        }
    }

    #[boa_fn]
    fn range(start: i64, end: i64) -> Vec<i64> {
        match start.cmp(&end) {
            Ordering::Equal => vec![],
            Ordering::Less => (start..end).collect(),
            Ordering::Greater => ((end + 1)..=start).rev().collect(),
        }
    }

    #[boa_fn]
    pub fn repeat(min: i64, max: Option<i64>) -> Vec<()> {
        let min = min as usize;
        let len = match max {
            Some(max) => thread_rng().gen_range(min..=(max as usize)),
            None => min,
        };
        vec![(); len]
    }

    #[boa_fn]
    fn replace(needle: &str, haystack: SJV, replacer: &str) -> SJV {
        let n = needle;
        let r = replacer;
        match haystack {
            SJV::String(s) => SJV::String(s.replace(n, r)),
            SJV::Array(a) => a
                .into_iter()
                .map(|v| replace(n, v, r))
                .collect::<Vec<_>>()
                .into(),
            SJV::Object(m) => SJV::Object(
                m.into_iter()
                    .map(|(k, v)| (k.replace(n, r), replace(n, v, r)))
                    .collect(),
            ),
            other => other,
        }
    }

    #[boa_fn]
    fn start_pad(s: AnyAsString, min_length: i64, pad_string: &str) -> String {
        use unicode_segmentation::UnicodeSegmentation;
        let s = s.get();
        let needed_chars = (min_length as usize).saturating_sub(s.len());

        let mut pad_chars: String = pad_string
            .graphemes(true)
            .cycle()
            .take(needed_chars)
            .collect();

        pad_chars.push_str(&s);
        pad_chars
    }

    mod helper {
        use crate::shared::{encode::Encoding, Epoch};
        use boa_engine::{object::JsArray, Context, JsResult, JsValue};
        use std::fmt::Display;

        pub(super) trait JsInput<'a>: Sized + 'a {
            fn from_js(js: &'a JsValue, ctx: &mut Context) -> JsResult<Self>;
        }

        impl JsInput<'_> for serde_json::Value {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                js.to_json(ctx)
            }
        }

        impl<'a, T: JsInput<'a>> JsInput<'a> for Option<T> {
            fn from_js(js: &'a JsValue, ctx: &mut Context) -> JsResult<Self> {
                Ok(T::from_js(js, ctx).ok())
            }
        }

        impl<'a> JsInput<'a> for &'a str {
            fn from_js(js: &'a JsValue, ctx: &mut Context) -> JsResult<Self> {
                Ok(js
                    .as_string()
                    .ok_or_else(|| ctx.construct_type_error("not a string"))?
                    .as_str())
            }
        }

        impl JsInput<'_> for i64 {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                match js {
                    JsValue::Integer(i) => Ok(*i as i64),
                    _ => Err(ctx.construct_type_error("not an int")),
                }
            }
        }

        impl JsInput<'_> for Encoding {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                let s: &str = JsInput::from_js(js, ctx)?;
                s.parse()
                    .map_err(|_| ctx.construct_type_error("invalid string for Encoding"))
            }
        }

        impl JsInput<'_> for Epoch {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                let s: &str = JsInput::from_js(js, ctx)?;
                s.parse()
                    .map_err(|_| ctx.construct_type_error("invalid string for Epoch"))
            }
        }

        pub(super) struct AnyAsString(String);

        impl JsInput<'_> for AnyAsString {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                Ok(Self(js.to_string(ctx)?.as_str().to_owned()))
            }
        }

        impl AnyAsString {
            pub fn get(self) -> String {
                self.0
            }
        }

        pub(super) enum NumType {
            Int(i64),
            Real(f64),
        }

        impl NumType {
            pub fn as_float(self) -> f64 {
                match self {
                    Self::Int(i) => i as f64,
                    Self::Real(f) => f,
                }
            }
        }

        impl JsInput<'_> for NumType {
            fn from_js(js: &JsValue, ctx: &mut Context) -> JsResult<Self> {
                match js {
                    JsValue::Integer(i) => Ok(Self::Int(*i as i64)),
                    JsValue::Rational(f) => Ok(Self::Real(*f)),
                    _ => Err(ctx.construct_type_error("needed numerical")),
                }
            }
        }

        pub(super) trait AsJsResult {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue>;
        }

        impl AsJsResult for serde_json::Value {
            fn as_js_result(self, ctx: &mut Context) -> JsResult<JsValue> {
                JsValue::from_json(&self, ctx)
            }
        }

        impl AsJsResult for f64 {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue> {
                Ok(JsValue::Rational(self))
            }
        }

        impl AsJsResult for i64 {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue> {
                Ok(JsValue::Integer(self as i32))
            }
        }

        impl AsJsResult for String {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue> {
                Ok(JsValue::String(self.into()))
            }
        }

        pub struct OrNull<T>(pub(super) Option<T>);

        impl<T> From<Option<T>> for OrNull<T> {
            fn from(value: Option<T>) -> Self {
                Self(value)
            }
        }

        impl<T: AsJsResult> AsJsResult for OrNull<T> {
            fn as_js_result(self, ctx: &mut Context) -> JsResult<JsValue> {
                Ok(self.0.as_js_result(ctx).unwrap_or(JsValue::Null))
            }
        }

        impl<T: AsJsResult> AsJsResult for Option<T> {
            fn as_js_result(self, ctx: &mut Context) -> JsResult<JsValue> {
                self.map(|x| x.as_js_result(ctx))
                    .transpose()?
                    .ok_or_else(|| JsValue::String("missing value".into()))
            }
        }

        impl<T: AsJsResult, E: Display> AsJsResult for Result<T, E> {
            fn as_js_result(self, ctx: &mut Context) -> JsResult<JsValue> {
                self.map_err(|e| JsValue::String(e.to_string().into()))
                    .and_then(|x| x.as_js_result(ctx))
            }
        }

        impl<T: AsJsResult> AsJsResult for Vec<T> {
            fn as_js_result(self, ctx: &mut Context) -> JsResult<JsValue> {
                Ok(JsArray::from_iter(
                    self.into_iter()
                        .map(|r| r.as_js_result(ctx))
                        .collect::<JsResult<Vec<_>>>()?,
                    ctx,
                )
                .into())
            }
        }

        impl AsJsResult for () {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue> {
                Ok(JsValue::Null)
            }
        }

        impl AsJsResult for NumType {
            fn as_js_result(self, _: &mut Context) -> JsResult<JsValue> {
                Ok(match self {
                    Self::Int(i) => JsValue::Integer(i as i32),
                    Self::Real(f) => JsValue::Rational(f),
                })
            }
        }
    }
}
