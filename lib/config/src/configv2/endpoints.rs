mod declare;
pub use declare::Declare;

use super::{
    common::{Duration, Headers, ProviderSend},
    load_pattern::LoadPattern,
    query::Query,
    templating::{Bool, False, Regular, Template, True, VarsOnly},
    PropagateVars,
};
use derive_more::{Deref, FromStr};
use serde::Deserialize;
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
    num::NonZeroUsize,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};
use thiserror::Error;

#[derive(Debug, Deserialize)]
pub struct Endpoint<VD: Bool = True> {
    #[serde(default = "BTreeMap::new")]
    pub declare: BTreeMap<Arc<str>, Declare<VD>>,
    #[serde(default = "Headers::new")]
    pub headers: Headers<VD>,
    pub body: Option<EndPointBody<VD>>,
    #[serde(bound = "LoadPattern<VD>: serde::de::DeserializeOwned")]
    pub load_pattern: Option<LoadPattern<VD>>,
    #[serde(default)]
    pub method: Method,
    pub peak_load: Option<Template<HitsPerMinute, VarsOnly, VD>>,
    #[serde(default = "BTreeMap::new")]
    pub tags: BTreeMap<String, Template<String, Regular, VD>>,
    pub url: Template<String, Regular, VD>,
    #[serde(default)]
    pub provides: BTreeMap<Arc<str>, EndpointProvides>,
    // book says optional, check what the behavior should be and if this
    // should default
    #[serde(default)]
    pub on_demand: bool,
    #[serde(default, with = "tuple_vec_map")]
    pub logs: Vec<(String, EndpointLogs)>,
    pub max_parallel_requests: Option<NonZeroUsize>,
    #[serde(default)]
    pub no_auto_returns: bool,
    pub request_timeout: Option<Template<Duration, VarsOnly, VD>>,
}

impl PropagateVars for Endpoint<False> {
    type Data<VD: Bool> = Endpoint<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        Ok(Endpoint {
            declare: self.declare.insert_vars(vars)?,
            headers: self.headers.insert_vars(vars)?,
            body: self.body.insert_vars(vars)?,
            load_pattern: self.load_pattern.insert_vars(vars)?,
            method: self.method,
            peak_load: self.peak_load.insert_vars(vars)?,
            tags: self.tags.insert_vars(vars)?,
            url: self.url.insert_vars(vars)?,
            provides: self.provides,
            on_demand: self.on_demand,
            logs: self.logs,
            max_parallel_requests: self.max_parallel_requests,
            no_auto_returns: self.no_auto_returns,
            request_timeout: self.request_timeout.insert_vars(vars)?,
        })
    }
}

impl Endpoint<True> {
    pub fn get_required_providers(&self) -> BTreeSet<Arc<str>> {
        self.declare
            .values()
            .flat_map(|v| v.get_required_providers().into_iter())
            .chain(
                self.headers
                    .iter()
                    .flat_map(|(_, h)| h.get_required_providers()),
            )
            .chain(
                self.body
                    .as_ref()
                    .map_or(BTreeSet::new(), |b| b.get_required_providers())
                    .into_iter(),
            )
            .chain(self.url.get_required_providers().into_iter())
            .collect()
    }

    /// Insert a load pattern if the current is None. The globally defined load_pattern should be
    /// used as a default if one is not defined locally.
    pub(crate) fn insert_load_pattern(&mut self, load: Option<&LoadPattern<True>>) {
        if let Some(lp) = load {
            self.load_pattern.get_or_insert_with(|| lp.clone());
        }
    }

    pub(crate) fn insert_special_tags(&mut self, id: usize) {
        let tags = &mut self.tags;
        tags.insert("_id".into(), Template::new_literal(id.to_string()));
        tags.insert(
            "method".into(),
            Template::new_literal(self.method.to_string()),
        );
        let url = &self.url;
        tags.entry("url".into())
            .or_insert_with(|| Template::new_literal(url.evaluate_with_star()));
    }

    pub(crate) fn insert_global_headers(&mut self, headers: &Headers<True>) {
        self.headers.extend(headers.iter().cloned())
    }
}

impl Endpoint<False> {
    pub fn insert_path(&mut self, path: &PathBuf) {
        if let Some(body) = self.body.as_mut() {
            body.add_file_path(path)
        }
    }
}

/// Newtype wrapper around [`http::Method`] for implementing [`serde::Deserialize`].
#[derive(Deserialize, Debug, Default, Deref, FromStr, PartialEq, Eq)]
#[serde(try_from = "&str")]
pub struct Method(http::Method);

impl TryFrom<&str> for Method {
    type Error = <Self as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum EndPointBody<VD: Bool = True> {
    #[serde(rename = "str")]
    String(Template<String, Regular, VD>),
    File(#[serde(skip)] PathBuf, Template<String, Regular, VD>),
    Multipart(#[serde(with = "tuple_vec_map")] Vec<(String, MultiPartBodySection<VD>)>),
}

impl EndPointBody<True> {
    fn get_required_providers(&self) -> BTreeSet<Arc<str>> {
        match self {
            Self::String(t) => t.get_required_providers(),
            Self::File(_, t) => t.get_required_providers(),
            Self::Multipart(m) => m
                .iter()
                .flat_map(|(_, s)| {
                    s.headers
                        .iter()
                        .flat_map(|(_, h)| h.get_required_providers().into_iter())
                        .chain(s.body.get_required_providers().into_iter())
                })
                .collect(),
        }
    }
}

impl EndPointBody<False> {
    fn add_file_path(&mut self, path: &PathBuf) {
        match self {
            Self::File(p, _) => *p = path.clone(),
            Self::Multipart(m) => m.iter_mut().for_each(|(_, s)| s.body.add_file_path(path)),
            _ => (),
        }
    }
}

impl PropagateVars for EndPointBody<False> {
    type Data<VD: Bool> = EndPointBody<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        use EndPointBody::*;
        match self {
            String(s) => s.insert_vars(vars).map(String),
            File(p, f) => f.insert_vars(vars).map(|f| File(p, f)),
            Multipart(mp) => mp.insert_vars(vars).map(Multipart),
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct MultiPartBodySection<VD: Bool = True> {
    #[serde(default = "Headers::new")]
    pub headers: Headers<VD>,
    pub body: EndPointBody<VD>,
}

impl PropagateVars for MultiPartBodySection<False> {
    type Data<VD: Bool> = MultiPartBodySection<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        let Self { headers, body } = self;
        Ok(MultiPartBodySection {
            headers: headers.insert_vars(vars)?,
            body: body.insert_vars(vars)?,
        })
    }
}

#[derive(Debug, Deserialize, PartialEq, PartialOrd, Deref)]
#[serde(try_from = "&str")]
pub struct HitsPerMinute(f64);

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParseHitsPerError {
    #[error("invalid hits per minute")]
    Invalid,
    #[error("hits per minute value too large")]
    TooBig,
}

impl FromStr for HitsPerMinute {
    type Err = ParseHitsPerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use crate::shared::Per;
        let (n, tag) = crate::shared::get_hits_per(s).ok_or(ParseHitsPerError::Invalid)?;
        // Highly doubt anyone will do this, but you never know.
        let n = n
            .is_finite()
            .then_some(n)
            .ok_or(ParseHitsPerError::TooBig)?;
        Ok(Self(
            n * match tag {
                Per::Minute => 1.0,
                Per::Second => 60.0,
            },
        ))
    }
}

impl TryFrom<&str> for HitsPerMinute {
    type Error = <Self as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

#[derive(Debug, Deserialize)]
pub struct EndpointProvides {
    query: Query,
    pub(crate) send: ProviderSend,
}

impl EndpointProvides {
    pub fn set_send_behavior(&mut self, send: ProviderSend) {
        self.send = send
    }
}

impl From<EndpointProvides> for (Query, ProviderSend) {
    fn from(EndpointProvides { query, send }: EndpointProvides) -> Self {
        (query, send)
    }
}

#[derive(Debug, Deserialize)]
#[serde(transparent)]
pub struct EndpointLogs {
    pub(crate) query: Query,
}

impl From<EndpointLogs> for (Query, ProviderSend) {
    fn from(value: EndpointLogs) -> Self {
        (value.query, ProviderSend::Block)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::configv2::False;
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn test_hits_per_minute() {
        assert_eq!("15hpm".parse(), Ok(HitsPerMinute(15.0)));
        assert_eq!("22 hpm".parse(), Ok(HitsPerMinute(22.0)));
        assert_eq!("1hps".parse(), Ok(HitsPerMinute(60.0)));

        assert_eq!("1.5 hpm".parse(), Ok(HitsPerMinute(1.5)));
        assert_eq!("0.5 hps".parse(), Ok(HitsPerMinute(30.0)));

        // Allowed, but should it be?
        assert_eq!("0hps".parse(), Ok(HitsPerMinute(0.0)));

        // Even though these are valid values for parsing a float, the regex won't catch them (and
        // shouldn't)
        assert_eq!(
            "NaN hpm".parse::<HitsPerMinute>(),
            Err(super::ParseHitsPerError::Invalid)
        );
        assert_eq!(
            "infinity hpm".parse::<HitsPerMinute>(),
            Err(super::ParseHitsPerError::Invalid)
        );
        assert_eq!(
            "-3.0 hpm".parse::<HitsPerMinute>(),
            Err(super::ParseHitsPerError::Invalid)
        );
    }

    #[test]
    fn test_body() {
        let EndPointBody::<False>::String(body) = from_yaml("!str my text").unwrap() else {
            panic!("was not template variant")
        };
        assert_eq!(
            body,
            Template::Literal {
                value: "my text".to_owned()
            }
        );

        // TODO: maybe find a way to not require a single value array here
        let EndPointBody::<False>::File(_, file) = from_yaml("!file [body.txt]").unwrap() else {
            panic!("was not file variant")
        };
        assert_eq!(
            file,
            Template::Literal {
                value: "body.txt".to_owned()
            }
        );

        static TEST: &str = r#"
        !multipart
          foo:
            headers:
              Content-Type: image/jpeg
            body:
              !file [foo.jpg]
          bar:
            body:
              !str some text"#;
        let EndPointBody::<False>::Multipart(multipart) = from_yaml(TEST).unwrap() else {
                    panic!("was not multipart variant")
                };
        assert_eq!(multipart.len(), 2);
        assert_eq!(multipart[0].0, "foo");
        assert_eq!(
            multipart[0].1,
            MultiPartBodySection {
                headers: vec![(
                    "Content-Type".to_owned(),
                    Template::Literal {
                        value: "image/jpeg".to_owned()
                    }
                )]
                .into(),
                body: EndPointBody::File(
                    Default::default(),
                    Template::Literal {
                        value: "foo.jpg".to_owned()
                    }
                )
            }
        );
        assert_eq!(multipart[1].0, "bar");
        assert_eq!(
            multipart[1].1,
            MultiPartBodySection {
                headers: Default::default(),
                body: EndPointBody::String(Template::Literal {
                    value: "some text".to_owned()
                })
            }
        );
    }

    #[test]
    fn test_method_default() {
        // The Default impl for the local Method is forwarded to http::Method::default()
        // in current version, that default is GET. This test is to check if that changes between
        // versions.
        assert_eq!(Method::default(), Method(http::Method::GET));
    }

    #[test]
    fn test_method() {
        // The pewpew book does not specify a valid subset, so assuming all should be tested.
        let Method(method) = from_yaml("GET").unwrap();
        assert_eq!(method, http::Method::GET);
        let Method(method) = from_yaml("CONNECT").unwrap();
        assert_eq!(method, http::Method::CONNECT);
        let Method(method) = from_yaml("DELETE").unwrap();
        assert_eq!(method, http::Method::DELETE);
        let Method(method) = from_yaml("HEAD").unwrap();
        assert_eq!(method, http::Method::HEAD);
        let Method(method) = from_yaml("OPTIONS").unwrap();
        assert_eq!(method, http::Method::OPTIONS);
        let Method(method) = from_yaml("PATCH").unwrap();
        assert_eq!(method, http::Method::PATCH);
        let Method(method) = from_yaml("POST").unwrap();
        assert_eq!(method, http::Method::POST);
        let Method(method) = from_yaml("PUT").unwrap();
        assert_eq!(method, http::Method::PUT);
        let Method(method) = from_yaml("TRACE").unwrap();
        assert_eq!(method, http::Method::TRACE);
    }

    #[test]
    fn test_endpoint() {
        static TEST: &str = r#"url: example.com"#;
        let Endpoint::<False> {
            declare,
            headers,
            body,
            load_pattern,
            method,
            peak_load,
            tags,
            url,
            provides,
            //on_demand,
            logs,
            max_parallel_requests,
            no_auto_returns,
            request_timeout,
            ..
        } = from_yaml(TEST).unwrap();
        assert!(declare.is_empty());
        assert!(headers.is_empty());
        assert_eq!(body, None);
        assert_eq!(load_pattern, None);
        assert_eq!(*method, http::Method::GET);
        assert_eq!(peak_load, None);
        assert!(tags.is_empty());
        assert_eq!(
            url,
            Template::Literal {
                value: "example.com".to_owned()
            }
        );
        assert!(provides.is_empty());
        //assert_eq!(on_demand, None);
        assert!(logs.is_empty());
        assert_eq!(max_parallel_requests, None);
        assert_eq!(no_auto_returns, false);
        assert_eq!(request_timeout, None);
    }
}
