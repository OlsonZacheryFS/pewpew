use boa_engine::{
    object::{JsArray, ObjectInitializer},
    property::Attribute,
    Context, JsResult, JsValue,
};
use derivative::Derivative;
use itertools::Itertools;
use serde::Deserialize;
use serde_json::Value as SJVal;
use std::{
    cell::{OnceCell, RefCell},
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

#[derive(Debug, Deserialize, Derivative)]
#[derivative(PartialEq, Eq)]
pub struct Query {
    select: Select,
    for_each: Vec<CompileOnce>,
    r#where: Option<CompileOnce>,
    #[serde(skip, default = "get_context")]
    #[derivative(PartialEq = "ignore")]
    ctx: RefCell<Context>,
}

fn get_context() -> RefCell<Context> {
    RefCell::from(Context::default()) // grab the scripting context if the functions are needed
}

impl Query {
    fn query(&self, data: Arc<SJVal>) -> impl Iterator<Item = SJVal> + Send {
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
                .map(|fe| fe.execute_on(ctx).unwrap())
                .collect_vec()
                .into_iter()
                .map(|jv| match jv {
                    JsValue::Object(o) if o.is_array() => {
                        let a = JsArray::from_object(o, ctx).unwrap();
                        let mut vd = VecDeque::with_capacity(a.length(ctx).unwrap() as usize);
                        std::iter::repeat_with(|| a.pop(ctx).unwrap())
                            .take_while(|v| !v.is_null_or_undefined())
                            .for_each(|v| vd.push_front(v));
                        vd
                    }
                    v => vec![v].into(),
                })
                .collect();
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
        for_each
            .into_iter()
            .filter_map(|x| {
                ctx.register_global_property("for_each", x, Attribute::READONLY);
                self.r#where
                    .as_ref()
                    .map_or(true, |w| w.execute_on(ctx).unwrap().as_boolean().unwrap())
                    .then(|| self.select.select(ctx).unwrap())
            })
            .collect_vec()
            .into_iter()
            .map(|x| x.to_json(ctx).unwrap())
            .collect_vec()
            .into_iter()
    }
}

#[derive(Debug, Deserialize, Derivative)]
#[derivative(PartialEq, Eq)]
struct CompileOnce(
    String,
    #[serde(skip)]
    #[derivative(PartialEq = "ignore")]
    OnceCell<gc::Gc<boa_engine::vm::CodeBlock>>,
);

impl From<String> for CompileOnce {
    fn from(value: String) -> Self {
        Self(value, OnceCell::new())
    }
}

impl CompileOnce {
    fn get(&self, ctx: &mut Context) -> gc::Gc<boa_engine::vm::CodeBlock> {
        self.1
            .get_or_init(|| {
                use boa_engine::syntax::Parser;

                let code = Parser::new(self.0.as_bytes()).parse_all(ctx).unwrap();
                ctx.compile(&code).unwrap()
            })
            .clone()
    }

    fn execute_on(&self, ctx: &mut Context) -> JsResult<JsValue> {
        let code = self.get(ctx);
        ctx.execute(code)
    }
}

#[derive(Debug, Deserialize, Derivative)]
#[derivative(PartialEq, Eq)]
#[serde(untagged)]
enum Select {
    Expr(CompileOnce),
    Map(BTreeMap<String, Self>),
}

impl Select {
    fn select(&self, ctx: &mut Context) -> JsResult<JsValue> {
        match self {
            Self::Expr(code) => code.execute_on(ctx),
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
        let q = Query {
            select: Select::Expr("response.body.session".to_owned().into()),
            r#where: Some("response.status < 400".to_owned().into()),
            for_each: vec![],
            ctx: get_context(),
        };
        let response = serde_json::json! { {"body": {"session": "abc123"}, "status": 200} };
        let res = q
            .query(Arc::new(serde_json::json!({ "response": response })))
            .collect_vec();
        assert_eq!(res, vec![SJVal::String("abc123".to_owned())]);

        let q = Query {
            select: Select::Map(
                [(
                    "name".to_owned(),
                    Select::Expr("for_each[0].name".to_owned().into()),
                )]
                .into(),
            ),
            r#where: Some("true".to_owned().into()),
            for_each: vec!["response.body.characters".to_owned().into()],
            ctx: get_context(),
        };
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
            .collect_vec();
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
