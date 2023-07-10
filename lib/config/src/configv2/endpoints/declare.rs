use crate::{
    configv2::PropagateVars,
    error::{EvalExprError, IntoStreamError, VarsError},
    scripting,
    templating::{Bool, False, Regular, Template, True},
};
use ether::Either;
use futures::{Stream, StreamExt, TryStreamExt};
use serde::Deserialize;
use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error as StdError,
    sync::Arc,
    task::Poll,
};

#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Declare<VD: Bool> {
    Expr(Template<String, Regular, VD>),
    Collect {
        collects: BTreeMap<String, Collect>,
        then: Template<String, Regular, VD>,
    },
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct Collect {
    take: Take,
    r#as: String,
}

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
            Self::Collect { collects, then } => Ok(Declare::Collect {
                collects,
                then: then.insert_vars(vars)?,
            }),
        }
    }
}

impl Declare<True> {
    pub fn get_required_providers(&self) -> BTreeSet<String> {
        match self {
            Self::Expr(t) => t.get_required_providers(),
            Self::Collect { collects, then } => {
                let ases: BTreeSet<_> = collects.values().map(|c| &c.r#as).collect();
                collects
                    .keys()
                    .cloned()
                    .chain(
                        then.get_required_providers()
                            .into_iter()
                            .filter(|k| !ases.contains(k)),
                    )
                    .collect()
            }
        }
    }
    pub fn into_stream<P, Ar, E>(
        self,
        providers: Arc<BTreeMap<String, P>>,
    ) -> Result<
        impl Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + 'static,
        IntoStreamError,
    >
    where
        P: scripting::ProviderStream<Ar, Err = E> + Clone + 'static,
        Ar: Clone + Send + Unpin + 'static,
        E: StdError + Send + Clone + Unpin + 'static + From<EvalExprError>,
    {
        match self {
            Self::Expr(t) => Ok(Either::A(t.into_stream(providers)?.map_ok(
                |(v, ar)| match v {
                    // The Templates will all yield Strings, so this allows any js type
                    serde_json::Value::String(s) => match serde_json::from_str(&s) {
                        Ok(v) => (v, ar),
                        Err(_) => (serde_json::Value::String(s), ar),
                    },
                    other => (other, ar),
                },
            ))),
            Self::Collect { collects, then } => {
                let collects = collects
                    .into_iter()
                    .map(|(from, Collect { take, r#as })| {
                        Ok((r#as, {
                            let p = providers
                                .get(&from)
                                .cloned()
                                .ok_or_else(|| IntoStreamError::MissingProvider(from))?;
                            move || {
                                use std::mem::{replace, take as mtake};
                                let mut p = p.as_stream();
                                let empty = move || Vec::with_capacity(take.max());
                                let mut cache = empty();
                                let mut ars = vec![];
                                let mut size = take.next_size();
                                Box::new(futures::stream::poll_fn(move |cx| {
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
                                        Poll::Ready(Some(Err(e))) => Poll::Ready(Some(Err(e))),
                                        Poll::Ready(None) => {
                                            Poll::Ready((!cache.is_empty()).then(|| {
                                                Ok((mtake(&mut cache).into(), mtake(&mut ars)))
                                            }))
                                        }
                                    }
                                }))
                            }
                        }))
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>()?;
                then.into_stream_with(move |p| {
                    providers.get(p).map(|p| p.as_stream()).or_else(|| {
                        collects.get(p).map(|p| {
                            p() as Box<
                                dyn Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>>
                                    + Send
                                    + Unpin,
                            >
                        })
                    })
                })
                .map(Either::B)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn basic() {
        let input = r#"expr"#;
        let d: Declare<True> = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(d, Declare::Expr(Template::new_literal("expr".into())));
        assert_eq!(d.get_required_providers(), Default::default());
        let input = r#""[${p:foo}]""#;
        let d: Declare<True> = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(d.get_required_providers(), ["foo".into()].into());
    }

    #[test]
    fn collect() {
        let input = r#"
        collects:
          foo:
            take: 5
            as: foo2
        then: "${p:foo2}${p:bar}"
        "#;
        let d: Declare<True> = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(
            d.get_required_providers(),
            ["foo".into(), "bar".into()].into()
        );
        let input = r#"
        collects: {}
        then: "${p:foo2}${p:bar}"
        "#;
        let d: Declare<True> = from_yaml::<Declare<False>>(input)
            .unwrap()
            .insert_vars(&BTreeMap::new())
            .unwrap();
        assert_eq!(
            d.get_required_providers(),
            ["foo2".into(), "bar".into()].into()
        );
    }
}
