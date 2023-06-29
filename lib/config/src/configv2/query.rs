use boa_engine::{
    object::{JsArray, ObjectInitializer},
    property::Attribute,
    vm::CodeBlock,
    Context, JsResult, JsValue,
};
use derivative::Derivative;
use diplomatic_bag::DiplomaticBag;
use gc::Gc;
use itertools::Itertools;
use serde::Deserialize;
use serde_json::Value as SJVal;
use std::{
    cell::RefCell,
    collections::{BTreeMap, VecDeque},
    convert::{TryFrom, TryInto},
    sync::Arc,
};

use crate::error::{EvalExprError, EvalExprErrorInner};

#[derive(Debug, Deserialize)]
#[serde(try_from = "QueryTmp")]
pub struct Query(DiplomaticBag<QueryInner>);

impl Query {
    pub fn query(
        &self,
        data: Arc<SJVal>,
    ) -> Result<impl Iterator<Item = Result<SJVal, EvalExprError>> + Send, EvalExprError> {
        self.0.as_ref().and_then(|_, q| {
            Ok(q.query(data)?
                .map(|i| i.map_err(Into::into))
                .collect_vec()
                .into_iter())
        })
    }

    pub fn simple(
        select: String,
        for_each: Vec<String>,
        r#where: Option<String>,
    ) -> Result<Self, &'static str> {
        QueryTmp {
            select: SelectTmp::Expr(select),
            for_each,
            r#where,
        }
        .try_into()
    }
}

impl TryFrom<QueryTmp> for Query {
    type Error = &'static str;

    fn try_from(value: QueryTmp) -> Result<Self, Self::Error> {
        DiplomaticBag::new(move |_| QueryInner::try_from(value))
            .transpose()
            .map_err(DiplomaticBag::into_inner)
            .map(Self)
    }
}

#[derive(Debug)]
struct QueryInner {
    select: Select,
    for_each: Vec<Gc<CodeBlock>>,
    r#where: Option<Gc<CodeBlock>>,
    ctx: RefCell<Context>,
}

#[derive(Debug, Deserialize, Derivative)]
struct QueryTmp {
    select: SelectTmp,
    #[serde(default = "Vec::new")]
    for_each: Vec<String>,
    r#where: Option<String>,
}

impl TryFrom<QueryTmp> for QueryInner {
    type Error = &'static str;

    fn try_from(value: QueryTmp) -> Result<Self, Self::Error> {
        let ctx = get_context();
        let select = value
            .select
            .compile(&mut ctx.borrow_mut())
            .ok_or("invalid select")?;
        let for_each = value
            .for_each
            .into_iter()
            .map(|fe| compile(&fe, &mut ctx.borrow_mut()))
            .collect::<Option<Vec<_>>>()
            .ok_or("invalid for_each")?;
        let r#where = value
            .r#where
            .map(|w| compile(&w, &mut ctx.borrow_mut()).ok_or("invalid where"))
            .transpose()?;

        Ok(Self {
            select,
            for_each,
            r#where,
            ctx,
        })
    }
}

fn compile(src: &str, ctx: &mut Context) -> Option<Gc<CodeBlock>> {
    use boa_engine::syntax::Parser;

    let code = Parser::new(src.as_bytes()).parse_all(ctx).ok()?;
    ctx.compile(&code).ok()
}

fn get_context() -> RefCell<Context> {
    RefCell::from(super::scripting::get_default_context())
}

impl QueryInner {
    fn query(
        &self,
        data: Arc<SJVal>,
    ) -> Result<impl Iterator<Item = Result<SJVal, EvalExprErrorInner>>, EvalExprErrorInner> {
        use EvalExprErrorInner::ExecutionError;
        let mut ctx = self.ctx.borrow_mut();
        let ctx = &mut ctx;
        let data = data.as_object().unwrap();
        let response = data.get("response");
        let request = data.get("request");
        let stats = data.get("stats");
        IntoIterator::into_iter([
            ("request", &request),
            ("response", &response),
            ("stats", &stats),
        ])
        .map(|(n, o)| {
            (
                n,
                o.and_then(|o| JsValue::from_json(o, ctx).ok())
                    .unwrap_or(JsValue::Undefined),
            )
        })
        .collect_vec()
        .into_iter()
        .for_each(|(n, o)| ctx.register_global_property(n, o, Attribute::READONLY));
        let for_each = {
            let for_each: Vec<VecDeque<JsValue>> = self
                .for_each
                .iter()
                .map(|fe| ctx.execute(fe.clone()).map_err(ExecutionError))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|jv| {
                    Ok(match jv {
                        JsValue::Object(o) if o.is_array() => {
                            let a = JsArray::from_object(o, ctx).map_err(ExecutionError)?;
                            let mut vd = VecDeque::with_capacity(a.length(ctx).unwrap() as usize);
                            while a.length(ctx).map_err(ExecutionError)? > 0 {
                                let v = a.pop(ctx).map_err(ExecutionError)?;
                                vd.push_front(v)
                            }
                            vd
                        }
                        v => vec![v].into(),
                    })
                })
                .collect::<Result<Vec<_>, EvalExprErrorInner>>()?;
            let for_each = for_each
                .into_iter()
                .multi_cartesian_product()
                .map(|v| JsArray::from_iter(v, ctx).into())
                .collect_vec();
            if for_each.is_empty() {
                vec![JsValue::Undefined]
            } else {
                for_each
            }
        };
        Ok(for_each
            .into_iter()
            .map(|x| {
                ctx.register_global_property("for_each", x, Attribute::READONLY);
                Ok(self
                    .r#where
                    .as_ref()
                    .map_or(Ok(true), |w| {
                        Ok(ctx.execute(w.clone()).map_err(ExecutionError)?.to_boolean())
                    })?
                    .then(|| self.select.select(ctx).map_err(ExecutionError)))
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .map(|x| {
                x.and_then(|v| {
                    v.to_json(ctx)
                        .map_err(EvalExprErrorInner::InvalidResultJson)
                })
            })
            .collect_vec()
            .into_iter())
    }
}

#[derive(Debug)]
enum Select {
    Expr(Gc<CodeBlock>),
    Map(BTreeMap<String, Self>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum SelectTmp {
    Expr(String),
    Map(BTreeMap<String, Self>),
}

impl SelectTmp {
    fn compile(self, ctx: &mut Context) -> Option<Select> {
        match self {
            Self::Expr(src) => compile(&src, ctx).map(Select::Expr),
            Self::Map(m) => m
                .into_iter()
                .map(|(k, v)| v.compile(ctx).map(|v| (k, v)))
                .collect::<Option<BTreeMap<_, _>>>()
                .map(Select::Map),
        }
    }
}

impl Select {
    fn select(&self, ctx: &mut Context) -> JsResult<JsValue> {
        match self {
            Self::Expr(code) => ctx.execute(code.clone()),
            Self::Map(m) => {
                let m: BTreeMap<&str, JsValue> = m
                    .iter()
                    .map(|(k, v)| Ok((k.as_str(), v.select(ctx)?)))
                    .collect::<JsResult<_>>()?;
                let mut obj = ObjectInitializer::new(ctx);
                for (k, v) in m {
                    obj.property(k, v, Attribute::READONLY);
                }

                Ok(obj.build().into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_queries() {
        let q = QueryTmp {
            select: SelectTmp::Expr("response.body.session".to_owned()),
            r#where: Some("response.status < 400".to_owned()),
            for_each: vec![],
        };
        let q = QueryInner::try_from(q).unwrap();
        let response = serde_json::json! { {"body": {"session": "abc123"}, "status": 200} };
        let res = q
            .query(Arc::new(serde_json::json!({ "response": response })))
            .unwrap()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(res, vec![SJVal::String("abc123".to_owned())]);

        let q = QueryInner::try_from(QueryTmp {
            select: SelectTmp::Map(
                [(
                    "name".to_owned(),
                    SelectTmp::Expr("for_each[0].name".to_owned()),
                )]
                .into(),
            ),
            r#where: Some("true".to_owned()),
            for_each: vec!["response.body.characters".to_owned()],
        })
        .unwrap();
        let response = serde_json::json! {
            {"body":    {
          "characters": [
            {
              "type": "Human",
              "id": "1000",
              "name": "Luke Skywalker",
              "friends": ["1002", "1003", "2000", "2001"],
              "appearsIn": [4, 5, 6],
              "homePlanet": "Tatooine",
            },
            {
              "type": "Human",
              "id": "1001",
              "name": "Darth Vader",
              "friends": ["1004"],
              "appearsIn": [4, 5, 6],
              "homePlanet": "Tatooine",
            },
            {
              "type": "Droid",
              "id": "2001",
              "name": "R2-D2",
              "friends": ["1000", "1002", "1003"],
              "appearsIn": [4, 5, 6],
              "primaryFunction": "Astromech",
            }
          ]
        }

        }};
        let res = q
            .query(Arc::new(serde_json::json!({ "response": response })))
            .unwrap()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(
            res,
            vec![
                serde_json::json!({"name": "Luke Skywalker"}),
                serde_json::json!({"name": "Darth Vader"}),
                serde_json::json!({"name": "R2-D2"})
            ]
        );
    }
}
