use crate::{
    configv2::PropagateVars,
    error::{EvalExprError, IntoStreamError, VarsError},
    scripting,
    templating::{Bool, False, Regular, Template, True, TryDefault},
};
use ether::Either;
use futures::{Stream, StreamExt, TryStreamExt};
use serde::Deserialize;
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
    error::Error as StdError,
    sync::Arc,
    task::Poll,
};

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub enum Declare<VD: Bool> {
    #[serde(rename = "x")]
    Expr(Template<String, Regular, VD>),
    #[serde(rename = "c")]
    Collects {
        collects: Vec<Collect<VD>>,
        then: Template<String, Regular, VD>,
    },
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct Collect<VD: Bool> {
    /// How many values to take.
    take: Take,
    /// Where to take values from.
    from: CollectSource<VD>,
    /// Name to refer to the resulting array as.
    r#as: String,
}

/// Specifies where values for a Collect are taken from.
#[derive(Debug, Deserialize, PartialEq, Eq)]
enum CollectSource<VD: Bool> {
    /// Take a var at the path.
    /// The same var value is cloned for as many times as needed.
    #[serde(rename = "v")]
    Var(VarSource<VD>),
    /// Pull multiple values from the specified provider.
    #[serde(rename = "p")]
    Prov(Arc<str>),
}

impl PropagateVars for CollectSource<False> {
    type Data<VD: Bool> = CollectSource<VD>;

    fn insert_vars(
        self,
        vars: &crate::configv2::Vars<True>,
    ) -> Result<Self::Data<True>, VarsError> {
        match self {
            Self::Prov(p) => Ok(Self::Data::Prov(p)),
            Self::Var(v) => v.insert_vars(vars).map(Self::Data::Var),
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(try_from = "String", bound = "")]
enum VarSource<VD: Bool> {
    Pre(String, VD::Inverse),
    Post(serde_json::Value, VD),
}

impl VarSource<True> {
    fn unwrap(self) -> serde_json::Value {
        match self {
            Self::Pre(_, no) => no.no(),
            Self::Post(v, True) => v,
        }
    }
}

impl<VD: Bool> TryFrom<String> for VarSource<VD> {
    type Error = &'static str;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        <VD::Inverse as TryDefault>::try_default()
            .map(|b| Self::Pre(value, b))
            .ok_or("wrong one")
    }
}

impl PropagateVars for VarSource<False> {
    type Data<VD: Bool> = VarSource<VD>;

    fn insert_vars(
        self,
        vars: &crate::configv2::Vars<True>,
    ) -> Result<Self::Data<True>, VarsError> {
        match self {
            Self::Pre(v, True) => crate::configv2::get_var_at_path(vars, &v)
                .cloned()
                .map(|v| Self::Data::Post(v.into(), True))
                .ok_or_else(|| crate::configv2::VarsError::VarNotFound(v)),
            Self::Post(_, no) => no.no(),
        }
    }
}

impl PropagateVars for Collect<False> {
    type Data<VD: Bool> = Collect<VD>;

    fn insert_vars(
        self,
        vars: &crate::configv2::Vars<True>,
    ) -> Result<Self::Data<True>, VarsError> {
        Ok(Collect {
            take: self.take,
            from: self.from.insert_vars(vars)?,
            r#as: self.r#as,
        })
    }
}

/// Data for how many values should be taken from underlying source each yield.
#[derive(Debug, Deserialize, PartialEq, Eq, Clone, Copy)]
#[serde(untagged)]
enum Take {
    Fixed(usize),
    Rand(usize, usize),
}

impl Take {
    fn next_size(&self) -> usize {
        use rand::prelude::*;
        match self {
            Self::Fixed(x) => *x,
            Self::Rand(min, max) => thread_rng().gen_range(*min..*max),
        }
    }

    /// Maximum size that this Take represents.
    /// Used to ensure single allocation of Vec
    fn max(&self) -> usize {
        match self {
            Self::Fixed(x) => *x,
            Self::Rand(_, max) => *max,
        }
    }
}

impl PropagateVars for Declare<False> {
    type Data<VD: Bool> = Declare<VD>;

    fn insert_vars(
        self,
        vars: &crate::configv2::Vars<True>,
    ) -> Result<Self::Data<True>, VarsError> {
        match self {
            Self::Expr(t) => t.insert_vars(vars).map(Self::Data::Expr),
            Self::Collects { collects, then } => Ok(Self::Data::Collects {
                collects: collects.insert_vars(vars)?,
                then: then.insert_vars(vars)?,
            }),
        }
    }
}

impl Declare<True> {
    pub fn get_required_providers(&self) -> BTreeSet<Arc<str>> {
        match self {
            Self::Expr(t) => t.get_required_providers(),
            Self::Collects { collects, then } => {
                let ases: BTreeSet<_> = collects.iter().map(|c| c.r#as.as_str()).collect();
                collects
                    .iter()
                    .filter_map(|c| match &c.from {
                        CollectSource::Var(_) => None,
                        CollectSource::Prov(p) => Some(p.clone()),
                    })
                    .chain(
                        then.get_required_providers()
                            .into_iter()
                            .filter(|k| !ases.contains::<str>(k)),
                    )
                    .collect()
            }
        }
    }

    pub fn into_stream<P, Ar, E>(
        self,
        providers: Arc<BTreeMap<Arc<str>, P>>,
    ) -> Result<
        impl Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + 'static,
        IntoStreamError,
    >
    where
        P: scripting::ProviderStream<Ar, Err = E> + Clone + 'static,
        Ar: Clone + Send + Unpin + 'static,
        E: StdError + Send + Clone + Unpin + 'static + From<EvalExprError>,
    {
        /// Kind of hacky workaround because PollFn is not Clone.
        ///
        /// Contains either a clonable stream, or a function that returns a Stream
        enum StreamOrFn<F: Fn() -> S2, S: Clone, S2> {
            Stream(S),
            F(F),
        }

        impl<F: Fn() -> S2, S: Clone, S2> StreamOrFn<F, S, S2> {
            fn get(&self) -> Either<S, S2> {
                match self {
                    Self::Stream(s) => Either::A(s.clone()),
                    Self::F(f) => Either::B(f()),
                }
            }
        }
        let stream = match self {
            Self::Expr(t) => t.into_stream(providers).map(Either::A),
            Self::Collects { collects, then } => {
                let collects = {
                    let providers = Arc::clone(&providers);
                    collects
                        .into_iter()
                        .map(move |Collect { take, from, r#as }| {
                            let stream = match from {
                                CollectSource::Var(v) => {
                                    let v = v.unwrap();
                                    Arc::new(StreamOrFn::Stream(futures::stream::repeat_with(
                                        // just keep cloning that var value into a vec
                                        // no polling is needed, as the vars are static
                                        move || {
                                            Ok((vec![v.clone(); take.next_size()].into(), vec![]))
                                        },
                                    )))
                                }
                                CollectSource::Prov(p) => {
                                    let providers = Arc::clone(&providers);
                                    let p = providers
                                        .get::<str>(&p)
                                        .cloned()
                                        .ok_or_else(|| IntoStreamError::MissingProvider(p))?;
                                    Arc::new(StreamOrFn::F(move || {
                                        use std::mem::{replace, take as mtake};

                                        let mut p = p.as_stream();

                                        let empty = move || Vec::with_capacity(take.max());
                                        // Vec to hold the values taken from the provider.
                                        let mut cache = empty();
                                        let mut ars = vec![];

                                        let mut size = take.next_size();
                                        // create stream to take multiple values
                                        futures::stream::poll_fn(move |cx| {
                                            match p.poll_next_unpin(cx) {
                                                Poll::Pending => Poll::Pending,
                                                Poll::Ready(Some(Ok((v, ar)))) => {
                                                    ars.extend(ar);
                                                    cache.push(v);
                                                    if cache.len() >= size {
                                                        size = take.next_size();
                                                        Poll::Ready(Some(Ok((
                                                            replace(&mut cache, empty()).into(),
                                                            mtake(&mut ars),
                                                        ))))
                                                    } else {
                                                        Poll::Pending
                                                    }
                                                }
                                                Poll::Ready(Some(Err(e))) => {
                                                    // Don't clear cache, because an Ok() may be
                                                    // yielded later.
                                                    Poll::Ready(Some(Err(e)))
                                                }
                                                Poll::Ready(None) => {
                                                    // Underlying stream has finished; yield any
                                                    // cached values, or return None.
                                                    Poll::Ready((!cache.is_empty()).then(|| {
                                                        Ok((
                                                            mtake(&mut cache).into(),
                                                            mtake(&mut ars),
                                                        ))
                                                    }))
                                                }
                                            }
                                        })
                                    }))
                                }
                            };
                            Ok((r#as, stream))
                        })
                        .collect::<Result<BTreeMap<_, _>, IntoStreamError>>()?
                };
                let providers = Arc::clone(&providers);
                then.into_stream_with(move |p| {
                    // Either get global provider, or local declare
                    providers.get(p).map(|p| p.as_stream()).or_else(|| {
                        collects.get(p).map(|p| {
                            Box::new(p.get())
                                as Box<
                                    dyn Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>>
                                        + Send
                                        + Unpin,
                                >
                        })
                    })
                })
                .map(Either::B)
            }
        };
        Ok(stream?.map_ok(|(v, ar)| {
            // Treat String as JSON if valid.
            let v = match v {
                serde_json::Value::String(s) => match serde_json::from_str(&s) {
                    Ok(v) => v,
                    Err(_) => serde_json::Value::String(s),
                },
                other => other,
            };
            (v, ar)
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::templating::ExprSegment;

    use super::*;
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn basic() {
        let input = "!x expr";

        let decl = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(decl.get_required_providers(), [].into());
        assert_eq!(decl, Declare::Expr(Template::new_literal("expr".into())));
    }

    #[test]
    fn collects() {
        let input = r#"!c
        collects:
          - take: 3
            from: !p a
            as: _a
        then: ${p:_a}"#;
        let decl = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(decl.get_required_providers(), ["a".into()].into());
        assert_eq!(
            decl,
            Declare::Collects {
                collects: vec![Collect {
                    take: Take::Fixed(3),
                    from: CollectSource::Prov("a".into()),
                    r#as: "_a".into()
                }],
                then: Template::NeedsProviders {
                    script: vec![ExprSegment::ProvDirect("_a".into())],
                    __dontuse: TryDefault::try_default().unwrap()
                }
            }
        );
        let input = r#"!c
        collects:
          - take: 3
            from: !p a
            as: _a
          - take: [4, 7]
            from: !p b
            as: _b
        then: ${p:_a}${p:_b}"#;
        let decl = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(
            decl.get_required_providers(),
            ["a".into(), "b".into()].into()
        );
        assert_eq!(
            decl,
            Declare::Collects {
                collects: vec![
                    Collect {
                        take: Take::Fixed(3),
                        from: CollectSource::Prov("a".into()),
                        r#as: "_a".into()
                    },
                    Collect {
                        take: Take::Rand(4, 7),
                        from: CollectSource::Prov("b".into()),
                        r#as: "_b".into()
                    }
                ],
                then: Template::NeedsProviders {
                    script: vec![
                        ExprSegment::ProvDirect("_a".into()),
                        ExprSegment::ProvDirect("_b".into())
                    ],
                    __dontuse: TryDefault::try_default().unwrap()
                }
            }
        );
    }
}
