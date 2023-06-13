use std::collections::{BTreeMap, VecDeque};

use boa_engine::{
    object::{JsArray, ObjectInitializer},
    property::Attribute,
    Context, JsResult, JsValue,
};
use itertools::Itertools;
use serde_json::Value as SJVal;

// TODO: check if strings can be precompiled here
// mainly: can you precompile a JS code that references the property "response" and execute it
// multiple times, even if "response" is a different value

struct Query {
    select: Select,
    for_each: Vec<String>,
    r#where: String,
}

impl Query {
    fn query(&self, request: SJVal, response: SJVal, stats: SJVal) -> Option<SJVal> {
        // maybe cache the same context (see TODO above)
        let mut ctx = Context::default(); // grab the scripting context if the functions are needed
        IntoIterator::into_iter([
            ("request", &request),
            ("response", &response),
            ("stats", &stats),
        ])
        .map(|(n, o)| (n, JsValue::from_json(o, &mut ctx).unwrap()))
        .collect::<Vec<_>>()
        .into_iter()
        .for_each(|(n, o)| ctx.register_global_property(n, o, Attribute::READONLY));
        ctx.eval(&self.r#where)
            .unwrap()
            .as_boolean()
            .unwrap()
            .then(|| {
                let for_each: Vec<VecDeque<JsValue>> = self
                    .for_each
                    .iter()
                    .map(|fe| ctx.eval(fe).unwrap())
                    .collect_vec()
                    .into_iter()
                    .map(|jv| match jv {
                        JsValue::Object(o) if o.is_array() => {
                            let a = JsArray::from_object(o, &mut ctx).unwrap();
                            let mut vd =
                                VecDeque::with_capacity(a.length(&mut ctx).unwrap() as usize);
                            std::iter::repeat_with(|| a.pop(&mut ctx).unwrap())
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
                    .map(|v| JsArray::from_iter(v, &mut ctx).into())
                    .collect_vec();
                let for_each = if for_each.is_empty() {
                    vec![JsValue::Undefined]
                } else {
                    for_each
                };

                let data = for_each
                    .into_iter()
                    .map(|x| {
                        ctx.register_global_property("for_each", x, Attribute::READONLY);
                        self.select.select(&mut ctx).unwrap()
                    })
                    .collect_vec();
                SJVal::Array(
                    data.into_iter()
                        .map(|x| x.to_json(&mut ctx).unwrap())
                        .collect(),
                )
            })
    }
}

enum Select {
    Expr(String),
    Map(BTreeMap<String, Self>),
}

impl Select {
    fn select(&self, ctx: &mut Context) -> JsResult<JsValue> {
        match self {
            Self::Expr(s) => ctx.eval(s),
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
            select: Select::Expr("response.body.session".to_owned()),
            r#where: "response.status < 400".to_owned(),
            for_each: vec![],
        };
        let response = serde_json::json! { {"body": {"session": "abc123"}, "status": 200} };
        let res = q.query(SJVal::Null, response, SJVal::Null).unwrap();
        assert_eq!(res, SJVal::Array(vec![SJVal::String("abc123".to_owned())]));

        let q = Query {
            select: Select::Map(
                [(
                    "name".to_owned(),
                    Select::Expr("for_each[0].name".to_owned()),
                )]
                .into(),
            ),
            r#where: "true".to_owned(),
            for_each: vec!["response.body.characters".to_owned()],
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
        let res = q.query(SJVal::Null, response, SJVal::Null).unwrap();
        assert_eq!(
            res,
            serde_json::json!([{"name": "Luke Skywalker"}, {"name": "Darth Vader"}, {"name": "R2-D2"}])
        );
    }
}
