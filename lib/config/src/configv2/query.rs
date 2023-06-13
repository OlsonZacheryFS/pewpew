use std::collections::VecDeque;

use boa_engine::{object::JsArray, property::Attribute, Context, JsValue};
use itertools::Itertools;
use serde_json::Value as SJVal;

struct Query {
    select: SJVal,
    for_each: Vec<String>,
    r#where: String,
}

impl Query {
    fn query(&self, request: SJVal, response: SJVal, stats: SJVal) -> Option<SJVal> {
        let mut ctx = Context::default();
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
                    vec![JsValue::Null]
                } else {
                    for_each
                };

                let data = for_each
                    .into_iter()
                    .map(|x| {
                        ctx.register_global_property("for_each", x, Attribute::READONLY);
                        ctx.eval(self.select.as_str().expect("only doing String selects now"))
                            .unwrap()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_queries() {
        let q = Query {
            select: SJVal::String("response.body.session".to_owned()),
            r#where: "response.status < 400".to_owned(),
            for_each: vec![],
        };
        let response = serde_json::json! { {"body": {"session": "abc123"}, "status": 200} };
        let res = q.query(SJVal::Null, response, SJVal::Null).unwrap();
        assert_eq!(res, SJVal::Array(vec![SJVal::String("abc123".to_owned())]));

        let q = Query {
            select: SJVal::String("for_each[0].name".to_owned()),
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
            serde_json::json!(["Luke Skywalker", "Darth Vader", "R2-D2"])
        );
    }
}
