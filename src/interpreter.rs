use std::collections::BTreeMap;

use crate::ast::{Expr, Stmt};
use crate::dsl_error::DSLError;
use crate::json::JsonValue;

pub struct Interpreter {
    vars: BTreeMap<String, JsonValue>,
    outputs: Vec<JsonValue>,
}

impl Interpreter {
    pub fn new(vars: BTreeMap<String, JsonValue>) -> Self {
        Self {
            vars,
            outputs: Vec::new(),
        }
    }

    pub fn run(&mut self, program: &[Stmt]) -> Result<JsonValue, DSLError> {
        for stmt in program {
            self.exec_stmt(stmt)?;
        }
        let mut out = BTreeMap::new();
        out.insert(
            "variables".to_string(),
            JsonValue::Object(self.vars.clone()),
        );
        out.insert(
            "outputs".to_string(),
            JsonValue::Array(self.outputs.clone()),
        );
        Ok(JsonValue::Object(out))
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), DSLError> {
        match stmt {
            Stmt::Assign { name, expr } => {
                let v = self.eval_expr(expr)?;
                self.vars.insert(name.clone(), v);
            }
            Stmt::Output { expr } => {
                let v = self.eval_expr(expr)?;
                self.outputs.push(v);
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                if truthy(&self.eval_expr(cond)?) {
                    for s in then_block {
                        self.exec_stmt(s)?;
                    }
                } else {
                    for s in else_block {
                        self.exec_stmt(s)?;
                    }
                }
            }
            Stmt::While { cond, body } => {
                while truthy(&self.eval_expr(cond)?) {
                    for s in body {
                        self.exec_stmt(s)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<JsonValue, DSLError> {
        match expr {
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Var(name) => self
                .vars
                .get(name)
                .cloned()
                .ok_or_else(|| DSLError(format!("Undefined variable: {name}"))),
            Expr::Unary { op, expr } => {
                let v = self.eval_expr(expr)?;
                match op.as_str() {
                    "-" => Ok(JsonValue::Number(-as_i64(&v)?)),
                    "not" => Ok(JsonValue::Bool(!truthy(&v))),
                    _ => Err(DSLError("Unknown unary operator".to_string())),
                }
            }
            Expr::Binary { op, left, right } => {
                let l = self.eval_expr(left)?;
                if op == "and" {
                    return Ok(JsonValue::Bool(
                        truthy(&l) && truthy(&self.eval_expr(right)?),
                    ));
                }
                if op == "or" {
                    return Ok(JsonValue::Bool(
                        truthy(&l) || truthy(&self.eval_expr(right)?),
                    ));
                }
                let r = self.eval_expr(right)?;
                apply_binary(op, &l, &r)
            }
        }
    }
}

fn truthy(v: &JsonValue) -> bool {
    match v {
        JsonValue::Bool(b) => *b,
        JsonValue::Number(n) => *n != 0,
        JsonValue::Null => false,
        JsonValue::String(s) => !s.is_empty(),
        JsonValue::Array(a) => !a.is_empty(),
        JsonValue::Object(o) => !o.is_empty(),
    }
}

fn as_i64(v: &JsonValue) -> Result<i64, DSLError> {
    v.as_i64()
        .ok_or_else(|| DSLError("Expected integer".to_string()))
}

fn apply_binary(op: &str, l: &JsonValue, r: &JsonValue) -> Result<JsonValue, DSLError> {
    Ok(match op {
        "+" => JsonValue::Number(as_i64(l)? + as_i64(r)?),
        "-" => JsonValue::Number(as_i64(l)? - as_i64(r)?),
        "*" => JsonValue::Number(as_i64(l)? * as_i64(r)?),
        "/" => JsonValue::Number(as_i64(l)? / as_i64(r)?),
        "==" => JsonValue::Bool(l == r),
        "!=" => JsonValue::Bool(l != r),
        "<" => JsonValue::Bool(as_i64(l)? < as_i64(r)?),
        "<=" => JsonValue::Bool(as_i64(l)? <= as_i64(r)?),
        ">" => JsonValue::Bool(as_i64(l)? > as_i64(r)?),
        ">=" => JsonValue::Bool(as_i64(l)? >= as_i64(r)?),
        _ => return Err(DSLError("Unknown binary operator".to_string())),
    })
}
