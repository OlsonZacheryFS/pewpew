use crate::common::ProviderSend;

use self::error::{InvalidForLoadTest, LoadTestGenError, MissingEnvVar, VarsError};
use self::templating::{Bool, EnvsOnly, False, Template, True};
use itertools::Itertools;
use serde::Deserialize;
use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    path::PathBuf,
};

pub mod config;
pub mod endpoints;
pub mod error;
pub mod load_pattern;
pub mod loggers;
pub mod providers;
pub mod query;
pub mod scripting;
pub mod templating;

pub mod common;

pub use self::config::{Config, General};
pub use common::Headers;
pub use endpoints::{EndPointBody, Endpoint};
pub use loggers::Logger;
pub use providers::ProviderType;

#[derive(Debug, Deserialize)]
pub struct LoadTest<VD: Bool = True, ED: Bool = True> {
    pub config: config::Config<VD>,
    #[serde(bound = "load_pattern::LoadPattern<VD>: serde::de::DeserializeOwned")]
    load_pattern: Option<load_pattern::LoadPattern<VD>>,
    vars: Vars<ED>,
    pub providers: BTreeMap<String, ProviderType<VD>>,
    pub loggers: BTreeMap<String, Logger<VD>>,
    pub endpoints: Vec<Endpoint<VD>>,
}

type Vars<ED> = BTreeMap<String, VarValue<ED>>;

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum VarValue<ED: Bool> {
    Nested(Vars<ED>),
    Terminal(VarTerminal<ED>),
}

impl VarValue<True> {
    fn get(&self, key: &str) -> Option<&Self> {
        match self {
            Self::Terminal(..) => None,
            Self::Nested(v) => v.get(key),
        }
    }

    fn finish(&self) -> Option<&VarTerminal<True>> {
        match self {
            Self::Terminal(v) => Some(v),
            Self::Nested(..) => None,
        }
    }
}

fn insert_env_vars(
    v: Vars<False>,
    evars: &BTreeMap<String, String>,
) -> Result<Vars<True>, MissingEnvVar> {
    v.into_iter()
        .map(|(k, v)| Ok((k, v.insert_env_vars(evars)?)))
        .collect()
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum VarTerminal<ED: Bool> {
    Num(i64),
    Bool(bool),
    Str(Template<String, EnvsOnly, True, ED>),
}

impl VarTerminal<False> {
    fn insert_env_vars(
        self,
        evars: &BTreeMap<String, String>,
    ) -> Result<VarTerminal<True>, MissingEnvVar> {
        match self {
            Self::Num(n) => Ok(VarTerminal::Num(n)),
            Self::Bool(b) => Ok(VarTerminal::Bool(b)),
            Self::Str(template) => template.insert_env_vars(evars).map(VarTerminal::Str),
        }
    }
}

impl VarValue<False> {
    fn insert_env_vars(
        self,
        evars: &BTreeMap<String, String>,
    ) -> Result<VarValue<True>, MissingEnvVar> {
        match self {
            Self::Nested(v) => insert_env_vars(v, evars).map(VarValue::Nested),
            Self::Terminal(t) => t.insert_env_vars(evars).map(VarValue::Terminal),
        }
    }
}

impl LoadTest<True, True> {
    pub fn from_yaml(
        yaml: &str,
        file_path: &PathBuf,
        env_vars: &BTreeMap<String, String>,
    ) -> Result<Self, LoadTestGenError> {
        let mut pre_vars =
            serde_yaml::from_str::<LoadTest<False, False>>(yaml)?.insert_env_vars(&env_vars)?;
        pre_vars
            .endpoints
            .iter_mut()
            .for_each(|e| e.insert_path(file_path));
        let vars = VarValue::Nested(std::mem::take(&mut pre_vars.vars));
        let mut lt = pre_vars.insert_vars(&vars)?;
        let lp = &lt.load_pattern;
        let ep = &mut lt.endpoints;
        ep.iter_mut()
            .for_each(|e| e.insert_load_pattern(lp.as_ref()));

        Ok(lt)
    }

    pub fn clear_loggers(&mut self) {
        self.loggers.clear();
        for endpoint in &mut self.endpoints {
            endpoint.logs.clear();
        }
    }

    pub fn add_logger(&mut self, name: String, l: Logger) -> Result<(), ()> {
        self.loggers.insert(name, l);
        // TODO: when should error?
        Ok(())
    }

    pub fn ok_for_loadtest(&self) -> Result<(), InvalidForLoadTest> {
        use InvalidForLoadTest::{MissingLoadPattern, MissingPeakLoad};
        let missing = self
            .endpoints
            .iter()
            .enumerate()
            .filter_map(|(i, e)| e.load_pattern.is_none().then_some(i))
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            return Err(MissingLoadPattern(missing));
        }
        let missing_peak = self
            .endpoints
            .iter()
            .enumerate()
            .filter(|(_, e)| e.peak_load.is_none())
            .filter(|(_, e)| {
                e.get_required_providers()
                    .into_iter()
                    .all(|p| match self.providers.get(&p) {
                        None => true,
                        Some(ProviderType::Response(_)) => false,
                        Some(_) => true,
                    })
            })
            .filter(|(_, e)| {
                e.provides
                    .iter()
                    .all(|(_, p)| p.send != ProviderSend::Block)
            })
            .map(|(i, _)| i)
            .collect_vec();

        if missing_peak.is_empty() {
            Ok(())
        } else {
            Err(MissingPeakLoad(missing_peak))
        }
    }

    pub fn get_duration(&self) -> std::time::Duration {
        self.endpoints
            .iter()
            .filter_map(|e| {
                e.load_pattern
                    .as_ref()
                    .map(load_pattern::LoadPattern::duration)
            })
            .max()
            .unwrap_or_default()
    }
}

impl LoadTest<False, False> {
    fn insert_env_vars(
        self,
        evars: &BTreeMap<String, String>,
    ) -> Result<LoadTest<False, True>, MissingEnvVar> {
        let Self {
            config,
            load_pattern,
            vars,
            providers,
            loggers,
            endpoints,
        } = self;
        Ok(LoadTest {
            config,
            load_pattern,
            vars: insert_env_vars(vars, evars)?,
            providers,
            loggers,
            endpoints,
        })
    }
}

/// Trait for inserting static Vars into Templates. Any type in the config that needs var values
/// should implement this trait.
///
/// `Self::Data<False>` should be the same type as `Self`
trait PropagateVars: Into<Self::Data<False>> {
    type Data<VD: Bool>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError>;
}

impl PropagateVars for LoadTest<False, True> {
    type Data<VD: Bool> = LoadTest<VD, True>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        let Self {
            config,
            load_pattern,
            vars: v,
            providers,
            loggers,
            endpoints,
        } = self;

        Ok(LoadTest {
            config: config.insert_vars(vars)?,
            load_pattern: load_pattern.insert_vars(vars)?,
            vars: v,
            providers: providers.insert_vars(vars)?,
            loggers: loggers.insert_vars(vars)?,
            endpoints: endpoints.insert_vars(vars)?,
        })
    }
}

impl<K, V> PropagateVars for BTreeMap<K, V>
where
    K: Ord,
    V: PropagateVars,
    BTreeMap<K, V::Data<False>>: From<Self>,
{
    type Data<VD: Bool> = BTreeMap<K, V::Data<VD>>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        self.into_iter()
            .map(|(k, v)| Ok((k, v.insert_vars(vars)?)))
            .collect()
    }
}

impl<K, V> PropagateVars for HashMap<K, V>
where
    K: Eq + Hash,
    V: PropagateVars,
    HashMap<K, V::Data<False>>: From<Self>,
{
    type Data<VD: Bool> = HashMap<K, V::Data<VD>>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        self.into_iter()
            .map(|(k, v)| Ok((k, v.insert_vars(vars)?)))
            .collect()
    }
}

impl<T> PropagateVars for Vec<T>
where
    T: PropagateVars,
    Vec<T::Data<False>>: From<Self>,
{
    type Data<VD: Bool> = Vec<T::Data<VD>>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        self.into_iter().map(|x| x.insert_vars(vars)).collect()
    }
}

impl<T> PropagateVars for Option<T>
where
    T: PropagateVars,
    Option<T::Data<False>>: From<Self>,
{
    type Data<VD: Bool> = Option<T::Data<VD>>;

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        self.map(|t| t.insert_vars(vars)).transpose()
    }
}

// used for the serde tuple vec map
impl<T, U> PropagateVars for (T, U)
where
    U: PropagateVars,
    (T, U::Data<False>): From<Self>,
{
    type Data<VD: Bool> = (T, U::Data<VD>);

    fn insert_vars(self, vars: &VarValue<True>) -> Result<Self::Data<True>, VarsError> {
        Ok((self.0, self.1.insert_vars(vars)?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints: []
        loggers: {}
        vars: {}
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        lt.ok_for_loadtest().unwrap();
    }

    #[test]
    fn error_missing_load_pattern() {
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints:
          - method: GET
            url: localhost:8000
        loggers: {}
        vars: {}
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        let err = lt.ok_for_loadtest().unwrap_err();
        assert_eq!(err, InvalidForLoadTest::MissingLoadPattern(vec![0]));
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints:
          - method: GET
            url: localhost:8000
          - method: POST
            url: localhost:9900
            load_pattern:
              - !linear
                  to: 150%
                  over: 5m
          - url: localhost:17777
        loggers: {}
        vars: {}
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        let err = lt.ok_for_loadtest().unwrap_err();
        assert_eq!(err, InvalidForLoadTest::MissingLoadPattern(vec![0, 2]));
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints:
          - peak_load: 12hpm
            url: localhost:8000
          - peak_load: 4hps
            url: localhost:9900
          - url: localhost:17777
            peak_load: 10hpm
        loggers: {}
        vars: {}
        load_pattern:
          - !linear
              to: 999%
              over: 2m
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        // global load pattern means endpoints do not need one
        lt.ok_for_loadtest().unwrap();
    }

    #[test]
    fn error_missing_peak_load() {
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints:
          - url: localhost:12345
        loggers: {}
        vars: {}
        load_pattern:
          - !linear
              to: 50%
              over: 1m
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        let err = lt.ok_for_loadtest().unwrap_err();
        assert_eq!(err, InvalidForLoadTest::MissingPeakLoad(vec![0]));

        let input = r#"
        config:
          client: {}
          general: {}
        providers:
          resp: !response
          a: !list
            - 1
        endpoints:
          # defines peak load
          - url: localhost:12345
            peak_load: 99hpm
          # depends on response provider
          - url: localhost:7777/${p:resp}
          # has a provides with block
          - url: localhost:23456
            provides:
              resp:
                query:
                  select: "1"
                send: block
              a:
                query:
                  select: "43"
                send: if_not_full
          # none of those
          - url: localhost:445${p:a}
        loggers: {}
        vars: {}
        load_pattern:
          - !linear
              to: 50%
              over: 1m
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        let err = lt.ok_for_loadtest().unwrap_err();
        assert_eq!(err, InvalidForLoadTest::MissingPeakLoad(vec![3]));
    }

    #[test]
    fn get_test_duration() {
        use std::time::Duration;
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints: []
        loggers: {}
        vars: {}
        load_pattern: []
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        assert_eq!(lt.get_duration(), Duration::default());
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints: []
        loggers: {}
        vars: {}
        load_pattern:
          - !linear
              from: 50%
              to: 150%
              over: 12h
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        assert_eq!(lt.get_duration(), Duration::default());
        let input = r#"
        config:
          client: {}
          general: {}
        providers: {}
        endpoints:
          - url: localhost:8080
            load_pattern:
              - !linear
                  to: 78%
                  over: 13h
          - url: localhost:9900
          - url: localhost:5432
            load_pattern:
              - !linear
                  to: 99%
                  over: 1m
        loggers: {}
        vars: {}
        load_pattern:
          - !linear
              from: 50%
              to: 150%
              over: 12h
        "#;
        let lt = LoadTest::from_yaml(input, &PathBuf::new(), &BTreeMap::new()).unwrap();
        assert_eq!(lt.get_duration(), Duration::from_secs(13 * 60 * 60));
    }
}
