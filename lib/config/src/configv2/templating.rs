//! Templating-related types. The generic type vars `VD` and `ED` correspond to "Vars Done", meaning
//! that static vars have been inserted, and "Envs Done", meaning that OS Environment variables have been
//! inserted.
//!
//! Rather than redefine nearly identical types multiple times for the same structure before and
//! after var processing, this module uses conditional enums to manage state, as well as which
//! template sources are allowed.
//!
//! Read here for more info: <https://rreverser.com/conditional-enum-variants-in-rust/>
//!
//! For example: `Template<_, EnvsOnly>` cannot be instantiated in the PreVars variant, because the
//! associated type is False.

use super::{
    error::{CreateExprError, EvalExprError, IntoStreamError, MissingEnvVar, VarsError},
    scripting::EvalExpr,
    PropagateVars,
};
use derivative::Derivative;
use ether::{Either, Either3};
use futures::{Stream, TryStreamExt};
pub use helpers::*;
use itertools::Itertools;
use serde::Deserialize;
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
    error::Error as StdError,
    iter::FromIterator,
    str::FromStr,
    sync::Arc,
};
use thiserror::Error;

mod parser;

pub use parser::Segment;

#[derive(Deserialize, PartialEq, Eq, Derivative)]
#[derivative(Debug)]
#[serde(try_from = "TemplatedString<T>")]
#[serde(bound = "")]
pub enum Template<
    V: FromStr,
    T: TemplateType,
    VD: Bool, /* = <<T as TemplateType>::VarsAllowed as Bool>::Inverse*/
    ED: Bool = <<T as TemplateType>::EnvsAllowed as Bool>::Inverse,
> where
    <V as FromStr>::Err: StdError + Send + Sync + 'static,
{
    Literal {
        value: V,
    },
    Env {
        template: TemplatedString<T>,
        #[derivative(Debug = "ignore")]
        __dontuse: (T::EnvsAllowed, ED::Inverse),
    },
    PreVars {
        template: TemplatedString<T>,
        /// Determines "next" state after vars propagation, depending on if initial variant needed
        /// provider values or not
        next: fn(TemplatedString<T>) -> Result<Template<V, T, True, True>, super::VarsError>,
        #[derivative(Debug = "ignore")]
        __dontuse: (T::VarsAllowed, VD::Inverse),
    },
    NeedsProviders {
        script: Vec<ExprSegment>,
        #[derivative(Debug = "ignore")]
        __dontuse: (ED, VD, T::ProvAllowed),
    },
}

impl<V, T, VD, ED> Clone for Template<V, T, VD, ED>
where
    V: FromStr + Clone,
    V::Err: StdError + Send + Sync + 'static,
    T: TemplateType<ProvAllowed = False>,
    VD: Bool,
    ED: Bool,
{
    fn clone(&self) -> Self {
        match self {
            Self::Literal { value } => Self::Literal {
                value: value.clone(),
            },
            Self::Env {
                template,
                __dontuse,
            } => Self::Env {
                template: template.clone(),
                __dontuse: *__dontuse,
            },
            Self::PreVars {
                template,
                next,
                __dontuse,
            } => Self::PreVars {
                template: template.clone(),
                next: *next,
                __dontuse: *__dontuse,
            },
            Self::NeedsProviders { __dontuse, .. } => __dontuse.2.no(),
        }
    }
}

#[derive(Debug)]
pub enum ExprSegment {
    Str(String),
    ProvDirect(String),
    Eval(EvalExpr),
}

impl PartialEq for ExprSegment {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for ExprSegment {}

#[derive(Debug, Error)]
pub enum TemplateGenError<V: FromStr>
where
    V::Err: StdError + 'static,
{
    #[error("{0}")]
    FromStr(#[source] V::Err),
}

impl<V, T, VD: Bool, ED: Bool> TryFrom<TemplatedString<T>> for Template<V, T, VD, ED>
where
    V: FromStr,
    V::Err: StdError + Send + Sync + 'static,
    T: TemplateType,
{
    type Error = TemplateGenError<V>;

    fn try_from(value: TemplatedString<T>) -> Result<Self, Self::Error> {
        match value.as_literal() {
            Ok(s) => match s.parse::<V>() {
                Ok(v) => Ok(Self::Literal { value: v }),
                Err(e) => Err(TemplateGenError::FromStr(e)),
            },
            Err(template) => {
                if T::EnvsAllowed::VALUE {
                    Ok(Self::Env {
                        template,
                        __dontuse: TryDefault::try_default()
                            .expect("should have already been checked"),
                    })
                } else {
                    Ok(Self::PreVars {
                        template,
                        next: if T::ProvAllowed::VALUE {
                            |s| {
                                let s = s.collapse();
                                match s.clone().try_collect() {
                                    None => Ok(Template::NeedsProviders {
                                        script: s.as_regular().unwrap().into_script()?,
                                        __dontuse: TryDefault::try_default().unwrap(),
                                    }),
                                    Some(s) => s
                                        .parse()
                                        .map_err(|e: <V as FromStr>::Err| {
                                            super::VarsError::InvalidString {
                                                typename: std::any::type_name::<V>(),
                                                from: s,
                                                error: Arc::new(e),
                                            }
                                        })
                                        .map(|v| Template::Literal { value: v }),
                                }
                            }
                        } else {
                            |s| {
                                let s = s.try_collect().unwrap();
                                s.parse()
                                    .map_err(|e: <V as FromStr>::Err| {
                                        super::VarsError::InvalidString {
                                            typename: std::any::type_name::<V>(),
                                            from: s,
                                            error: Arc::new(e),
                                        }
                                    })
                                    .map(|v| Template::Literal { value: v })
                            }
                        },
                        __dontuse: TryDefault::try_default()
                            .expect("should already have been checked"),
                    })
                }
            }
        }
    }
}

impl<T: TemplateType<ProvAllowed = True>> Template<String, T, True, True> {
    pub fn into_stream<P, Ar, E>(
        self,
        providers: &BTreeMap<String, P>,
    ) -> Result<
        impl Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + 'static,
        IntoStreamError,
    >
    where
        P: super::scripting::ProviderStream<Ar, Err = E> + 'static,
        Ar: Clone + Send + Unpin + 'static,
        E: StdError + Send + Clone + Unpin + 'static + From<EvalExprError>,
    {
        use futures::stream::repeat;
        Ok(match self {
            Self::Literal { value } => Either::A(repeat(Ok((
                serde_json::Value::String(value),
                vec![], // TODO: what is the Vec<Ar> for?
            )))),
            Self::NeedsProviders { script, .. } => {
                let streams = script
                    .into_iter()
                    .map(|s| match s {
                        ExprSegment::Str(s) => Ok(Either3::A(repeat(Ok((
                            serde_json::Value::String(s),
                            vec![],
                        ))))),
                        ExprSegment::ProvDirect(p) => providers
                            .get(&p)
                            .map(|p| Either3::B(p.as_stream()))
                            .ok_or(IntoStreamError::MissingProvider(p)),
                        ExprSegment::Eval(x) => x.into_stream(providers).map(Either3::C),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Either::B(zip_all::zip_all(streams).map_ok(|js| {
                    js.into_iter()
                        .map(|(j, ar)| (j.to_string(), ar))
                        .reduce(|mut acc, e| {
                            acc.0.push_str(&e.0);
                            acc.1.extend(e.1);
                            acc
                        })
                        .map(|(s, ar)| (serde_json::Value::String(s), ar))
                        .unwrap_or_default()
                }))
            }
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
        })
    }

    pub fn evaluate(&self, data: Cow<'_, serde_json::Value>) -> Result<String, EvalExprError> {
        match self {
            Self::Literal { value } => Ok(value.clone()),
            Self::NeedsProviders { script, __dontuse } => script
                .iter()
                .map(|e| match e {
                    ExprSegment::Eval(x) => x.evaluate(data.clone()),
                    ExprSegment::Str(s) => Ok(s.to_owned()),
                    ExprSegment::ProvDirect(p) => data
                        .as_object()
                        .and_then(|o| o.get(p.as_str()))
                        .map(ToString::to_string)
                        .ok_or_else(|| EvalExprError(format!("provider data {p} not found"))),
                })
                .collect(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
        }
    }

    pub fn evaluate_with_star(&self) -> String {
        match self {
            Self::Literal { value } => value.clone(),
            Self::NeedsProviders { script, __dontuse } => script
                .iter()
                .map(|x| match x {
                    ExprSegment::Str(s) => s.as_str(),
                    ExprSegment::Eval(_) | ExprSegment::ProvDirect(_) => "*",
                })
                .collect(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
        }
    }

    pub fn as_static(&self) -> Option<&str> {
        match self {
            Self::Literal { value } => Some(value),
            _ => None,
        }
    }

    pub fn get_required_providers(&self) -> BTreeSet<String> {
        match self {
            Self::Literal { .. } => BTreeSet::new(),
            Self::NeedsProviders { script, .. } => script
                .iter()
                .flat_map(|p| match p {
                    ExprSegment::Eval(x) => x.required_providers().into_iter().collect_vec(),
                    ExprSegment::ProvDirect(p) => vec![p.as_str()],
                    ExprSegment::Str(_) => vec![],
                })
                .collect::<BTreeSet<&str>>()
                .into_iter()
                .map(ToOwned::to_owned)
                .collect(),
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
        }
    }
}

impl<VD: Bool> Template<String, EnvsOnly, VD, False> {
    pub(crate) fn insert_env_vars(
        self,
        evars: &BTreeMap<String, String>,
    ) -> Result<Template<String, EnvsOnly, VD, True>, MissingEnvVar> {
        match self {
            Self::Literal { value } => Ok(Template::Literal { value }),
            Self::Env {
                template,
                __dontuse,
            } => Ok(Template::Literal {
                value: template
                    .insert_env_vars(evars)?
                    .try_collect()
                    .expect("EnvsOnly shouldn't have other types"),
            }),
            Self::PreVars { __dontuse, .. } => __dontuse.0.no(),
            Self::NeedsProviders { __dontuse, .. } => __dontuse.0.no(),
        }
    }
}

impl<V: FromStr, T: TemplateType<ProvAllowed = False>> Template<V, T, True, True>
where
    <V as FromStr>::Err: StdError + Send + Sync + 'static,
{
    pub fn get(&self) -> &V {
        match self {
            Self::Literal { value } => value,
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
            Self::NeedsProviders { __dontuse, .. } => __dontuse.2.no(),
        }
    }

    pub fn get_mut(&mut self) -> &mut V {
        match self {
            Self::Literal { value } => value,
            Self::PreVars { __dontuse, .. } => __dontuse.1.no(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
            Self::NeedsProviders { __dontuse, .. } => __dontuse.2.no(),
        }
    }
}

impl<V, T, VD, ED> Template<V, T, VD, ED>
where
    V: FromStr,
    T: TemplateType,
    VD: Bool,
    ED: Bool,
    V::Err: StdError + Send + Sync + 'static,
{
    pub fn new_literal(value: V) -> Self {
        Self::Literal { value }
    }
}

impl<V: FromStr, T: TemplateType<VarsAllowed = True, EnvsAllowed = False>> PropagateVars
    for Template<V, T, False, True>
where
    V::Err: StdError + Send + Sync + 'static,
{
    type Data<VD: Bool> = Template<V, T, VD, True>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, VarsError> {
        match self {
            Self::Literal { value } => Ok(Template::Literal { value }),
            Self::PreVars {
                template,
                next,
                __dontuse,
            } => {
                let s = template.insert_vars(vars)?.collapse();
                next(s)
            }
            Self::NeedsProviders { __dontuse, .. } => __dontuse.1.no(),
            Self::Env { __dontuse, .. } => __dontuse.1.no(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
#[serde(try_from = "&str")]
#[serde(bound = "")]
pub struct TemplatedString<T: TemplateType>(Vec<Segment<T>>);

impl<T: TemplateType> TemplatedString<T> {
    fn try_collect(self) -> Option<String> {
        self.0
            .into_iter()
            .map(|p| match p {
                parser::Segment::Raw(s) => Some(s),
                _ => None,
            })
            .collect()
    }

    fn collapse(self) -> Self {
        self.into_iter()
            .coalesce(|a, b| match (a, b) {
                (Segment::Raw(x), Segment::Raw(y)) => Ok(Segment::Raw(x + &y)),
                (x, y) => Err((x, y)),
            })
            .collect()
    }

    fn as_literal(mut self) -> Result<String, Self> {
        let one = self.0.pop();
        match (one, self.0.len()) {
            (Some(Segment::Raw(s)), 0) => Ok(s),
            (Some(seg), _) => {
                self.0.push(seg);
                Err(self)
            }
            (None, _) => Ok("".to_owned()),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &parser::Segment<T>> {
        self.0.iter()
    }

    fn as_regular(self) -> Option<TemplatedString<Regular>> {
        fn map_segment<T: TemplateType, I: Bool>(s: Segment<T, I>) -> Option<Segment<Regular, I>> {
            Some(match s {
                Segment::Raw(s) => Segment::Raw(s),
                Segment::Expr(x, _) => Segment::Expr(
                    x.into_iter().map(map_segment).collect::<Option<_>>()?,
                    TryDefault::try_default()?,
                ),
                Segment::Prov(p, _) => Segment::Prov(p, TryDefault::try_default()?),
                Segment::Env(e, _) => Segment::Env(e, TryDefault::try_default()?),
                Segment::Var(v, _) => Segment::Var(v, TryDefault::try_default()?),
            })
        }
        self.into_iter()
            .map(map_segment)
            .collect::<Option<TemplatedString<Regular>>>()
    }
}

impl<T: TemplateType<ProvAllowed = True, EnvsAllowed = False>> TemplatedString<T> {
    // only call after Vars insertion
    fn into_script(self) -> Result<Vec<ExprSegment>, CreateExprError>
    where
        T::ProvAllowed: OK,
    {
        self.into_iter()
            .map(|s| {
                Ok(match s {
                    Segment::Raw(x) => ExprSegment::Str(x),
                    Segment::Prov(p, _) => ExprSegment::ProvDirect(p),
                    Segment::Expr(x, _) => ExprSegment::Eval(EvalExpr::from_template(x)?),
                    _ => unreachable!("need to insert vars first"),
                })
            })
            .collect()
    }
}

impl<T: TemplateType> IntoIterator for TemplatedString<T> {
    type Item = Segment<T>;
    type IntoIter = <Vec<Segment<T>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: TemplateType> FromIterator<Segment<T>> for TemplatedString<T> {
    fn from_iter<I: IntoIterator<Item = Segment<T>>>(iter: I) -> Self {
        Self(iter.into_iter().collect_vec())
    }
}

impl<T: TemplateType<VarsAllowed = True, EnvsAllowed = False>> PropagateVars
    for TemplatedString<T>
{
    type Data<VD: Bool> = Self;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        self.0
            .into_iter()
            .map(|p| match p {
                Segment::Var(v, True) => super::get_var_at_path(vars, &v)
                    .ok_or_else(|| super::VarsError::VarNotFound(v))
                    .map(|v| Segment::Raw(v.to_string())),
                Segment::Env(_, no) => no.no(),
                Segment::Expr(v, True) => Ok(Segment::Expr(
                    v.into_iter()
                        .map(|s| match s {
                            Segment::Env(_, no) => no.no(),
                            Segment::Expr(_, no) => no.no(),
                            Segment::Var(v, True) => super::get_var_at_path(vars, &v)
                                .ok_or_else(|| super::VarsError::VarNotFound(v))
                                .map(|v| Segment::Raw(v.to_string())),
                            other => Ok(other),
                        })
                        .collect::<Result<_, _>>()?,
                    True,
                )),
                other => Ok(other),
            })
            .collect()
    }
}

impl<T: TemplateType<EnvsAllowed = True>> TemplatedString<T> {
    fn insert_env_vars(self, evars: &BTreeMap<String, String>) -> Result<Self, MissingEnvVar> {
        self.0
            .into_iter()
            .map(|p| match p {
                Segment::Env(e, ..) => evars
                    .get(&e)
                    .cloned()
                    .map(Segment::Raw)
                    .ok_or_else(|| MissingEnvVar(e)),
                Segment::Expr(x, True) => x
                    .into_iter()
                    .map(|s| match s {
                        Segment::Expr(_, no) => no.no(),
                        Segment::Env(e, True) => evars
                            .get(&e)
                            // make the String into a valid code literal
                            .map(|ev| Segment::Raw(format!("\"{}\"", ev.escape_default())))
                            .ok_or_else(|| MissingEnvVar(e)),
                        other => Ok(other),
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .map(|x| Segment::Expr(x, True)),
                other => Ok(other),
            })
            .collect()
    }
}

impl<T: TemplateType> FromStr for TemplatedString<T> {
    type Err = parser::TemplateParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parser::parse_template_string(s).map(Self)
    }
}

impl<T: TemplateType> TryFrom<&str> for TemplatedString<T> {
    type Error = <Self as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl<T, U> TryDefault for (T, U)
where
    T: TryDefault,
    U: TryDefault,
{
    fn try_default() -> Option<Self> {
        Some((T::try_default()?, U::try_default()?))
    }
}

impl<T, U, V> TryDefault for (T, U, V)
where
    T: TryDefault,
    U: TryDefault,
    V: TryDefault,
{
    fn try_default() -> Option<Self> {
        Some((T::try_default()?, U::try_default()?, V::try_default()?))
    }
}

mod helpers {
    use serde::Deserialize;
    use std::fmt;

    mod private {
        pub trait Seal {}

        impl Seal for super::True {}
        impl Seal for super::False {}
        impl Seal for super::EnvsOnly {}
        impl Seal for super::VarsOnly {}
        impl Seal for super::Regular {}
        impl<T, U> Seal for (T, U)
        where
            T: Seal,
            U: Seal,
        {
        }
        impl<T, U, V> Seal for (T, U, V)
        where
            T: Seal,
            U: Seal,
            V: Seal,
        {
        }
    }

    /// Unit type that only exists to allow enum variants containing to be made.
    #[derive(Default, Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
    pub struct True;

    /// Uninhabited type that makes enum variants containing it to be inaccessible.
    #[derive(Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
    pub enum False {}

    impl False {
        // last line is only unreachable in debug builds
        #[allow(unreachable_code)]
        pub fn no(&self) -> ! {
            #[cfg(debug_assertions)]
            {
                log::error!("somthing has gone horribly wrong");
                panic!("managed to call no() on a False");
            }
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

    /// Trait for trying to get a Default value. Serde itself has no solution (that I could find)
    /// that directly allows making specific enum variants inaccessible, so this is to make
    /// generating a Default value fallible based on the type. If an invaild variant is used (for
    /// example, an env variant for a template outside of the vars section), then
    /// `False::try_default()` will be called, and an error will be forwarded and Deserialize will
    /// fail.
    pub trait TryDefault: Sized + fmt::Debug + private::Seal {
        fn try_default() -> Option<Self>;
    }

    impl TryDefault for True {
        fn try_default() -> Option<Self> {
            Some(Self)
        }
    }

    impl TryDefault for False {
        fn try_default() -> Option<Self> {
            None
        }
    }

    /// Trait for a type that represents a boolean state for if a value can be constructed.
    pub trait Bool:
        fmt::Debug + TryDefault + Clone + Copy + PartialEq + Eq + private::Seal
    {
        type Inverse: Bool + fmt::Debug;

        const VALUE: bool;
    }

    /// Trait meaning that the Boolean type specifically can be created.
    pub trait OK: Default + Bool + private::Seal {}

    impl OK for True {}

    impl Bool for True {
        type Inverse = False;
        const VALUE: bool = true;
    }

    impl Bool for False {
        type Inverse = True;
        const VALUE: bool = false;
    }

    /// Trait for types of templatings allowed. It's not an enumeration of variants, because
    /// Template needs to be generic over a type of this trait.
    pub trait TemplateType: fmt::Debug + private::Seal + PartialEq + Eq + Clone + Copy {
        type EnvsAllowed: Bool;
        type VarsAllowed: Bool;
        type ProvAllowed: Bool;
    }

    /// Marker struct to indicate that this template can only read from OS environment variables as
    /// a source.
    #[derive(Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
    pub struct EnvsOnly;

    impl TemplateType for EnvsOnly {
        type EnvsAllowed = True;
        type VarsAllowed = False;
        type ProvAllowed = False;
    }

    /// Marker struct to indicate that this template can only read from static Vars as a source.
    #[derive(Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
    pub struct VarsOnly;

    impl TemplateType for VarsOnly {
        type EnvsAllowed = False;
        type VarsAllowed = True;
        type ProvAllowed = False;
    }

    /// Marker struct to indicate that this template can read from vars or providers.
    #[derive(Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Regular;

    impl TemplateType for Regular {
        type EnvsAllowed = False;
        type VarsAllowed = True;
        type ProvAllowed = True;
    }
}
