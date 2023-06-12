use crate::configv2::templating::TemplatePiece;

use super::templating::{Template, TemplateType, True, OK};
use boa_engine::{
    object::{JsFunction, ObjectInitializer},
    prelude::*,
    property::Attribute,
};
use futures::{Stream, TryStreamExt};
use itertools::Itertools;
use std::collections::BTreeMap;
use thiserror::Error;
use zip_all::zip_all_map;

type ProviderStreamStream<Ar, E> =
    Box<dyn Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>> + Send + Unpin + 'static>;

pub trait ProviderStream<Ar: Clone + Send + Unpin + 'static> {
    type Err: Unpin + std::error::Error;

    fn as_stream(&self) -> ProviderStreamStream<Ar, Self::Err>;
}

struct EvalExpr {
    ctx: Context,
    efn: JsFunction,
    needed: Vec<String>,
}

impl EvalExpr {
    fn from_template<T>(template: Template<String, T, True, True>) -> Result<Self, CreateExprError>
    where
        T: TemplateType,
        T::ProvAllowed: OK,
    {
        let Template::NeedsProviders { script, .. } = template else {
            return Err(CreateExprError::LiteralForTemplate);
        };

        let mut needed = Vec::new();
        let script = format!(
            "function ____eval(____provider_values){{ return {}; }}",
            script
                .into_iter()
                .map(|p| match p {
                    TemplatePiece::Raw(s) => s,
                    TemplatePiece::Provider(p, ..) => {
                        let s = format!("____provider_values.{p}");
                        needed.push(p);
                        s
                    }
                    _ => unreachable!(),
                })
                .collect::<String>()
        );
        let mut ctx = default_context();
        ctx.eval(script).map_err(CreateExprError::BuildFnFailure)?;
        let efn: JsFunction =
            JsFunction::from_object(ctx.eval("____eval").unwrap().as_object().unwrap().clone())
                .unwrap();
        Ok(Self { ctx, efn, needed })
    }

    fn into_stream<P, Ar, E>(
        mut self,
        providers: &BTreeMap<String, P>,
    ) -> Result<impl Stream<Item = Result<(serde_json::Value, Vec<Ar>), E>>, IntoStreamError>
    where
        P: ProviderStream<Ar, Err = E> + Sized + 'static,
        Ar: Clone + Send + Unpin + 'static,
        E: Unpin + std::error::Error,
    {
        let providers = std::mem::take(&mut self.needed)
            .into_iter()
            .map(|pn| {
                providers
                    .get(&pn)
                    .map(|p| (pn.clone(), p.as_stream()))
                    .ok_or_else(|| IntoStreamError::MissingProvider(pn.clone()))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;
        Ok(zip_all_map(providers, true).map_ok(move |values| {
            let ctx = &mut self.ctx;

            let values: BTreeMap<_, _> = values
                .into_iter()
                .map(|(n, (v, ar))| {
                    JsValue::from_json(&v, ctx)
                        .map_err(EvalExprError::InvalidJsonFromProvider)
                        .map(|v| (n, (v, ar)))
                })
                .collect::<Result<_, _>>()
                .unwrap();
            let mut object = ObjectInitializer::new(ctx);
            for (name, (value, _)) in values.iter() {
                object.property(name.as_str(), value, Attribute::READONLY);
            }
            let object = object.build();
            (
                self.efn
                    .call(&JsValue::Null, &[object.into()], ctx)
                    .unwrap()
                    .to_json(ctx)
                    .unwrap(),
                values.into_iter().flat_map(|v| v.1 .1).collect_vec(),
            )
        }))
    }
}

#[derive(Debug, Error)]
enum CreateExprError {
    #[error("template provided was an already evaluated literal value")]
    LiteralForTemplate,
    #[error("failure building JS function: {}", .0.display())]
    BuildFnFailure(JsValue),
}

#[derive(Debug, Error)]
enum IntoStreamError {
    #[error("missing provider: {0}")]
    MissingProvider(String),
}

#[derive(Debug, Error)]
enum EvalExprError {
    #[error("provider returned invalid json: {}", .0.display())]
    InvalidJsonFromProvider(JsValue),
}

fn default_context() -> Context {
    Context::default()
}
