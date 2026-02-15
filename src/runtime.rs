use std::collections::BTreeMap;

use crate::dsl_error::DSLError;
use crate::interpreter::Interpreter;
use crate::json::{JsonParser, JsonValue};
use crate::lexer::Lexer;
use crate::parser::Parser;

pub fn run_program(
    source: &str,
    input: BTreeMap<String, JsonValue>,
) -> Result<JsonValue, DSLError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;
    let mut interpreter = Interpreter::new(input);
    interpreter.run(&program)
}

pub fn parse_payload(input: &str) -> Result<(String, BTreeMap<String, JsonValue>), DSLError> {
    let root = JsonParser::new(input).parse()?;
    let obj = root
        .as_object()
        .ok_or_else(|| DSLError("Input payload must be a JSON object".to_string()))?;
    let program = match obj.get("program") {
        Some(JsonValue::String(s)) => s.clone(),
        _ => return Err(DSLError("`program` must be a string".to_string())),
    };
    let vars = match obj.get("input") {
        None => BTreeMap::new(),
        Some(JsonValue::Object(map)) => map.clone(),
        Some(_) => return Err(DSLError("`input` must be a JSON object".to_string())),
    };
    Ok((program, vars))
}

pub fn error_json(message: &str) -> String {
    let mut obj = BTreeMap::new();
    obj.insert("error".to_string(), JsonValue::String(message.to_string()));
    JsonValue::Object(obj).to_json()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic_and_output() {
        let result = run_program("x = 3 + 4 * 2; output x;", BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(11)]))
        );
    }

    #[test]
    fn if_else_and_boolean_operators() {
        let program = "flag = true and not false; if flag { output 1; } else { output 0; }";
        let result = run_program(program, BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(1)]))
        );
    }

    #[test]
    fn while_loop() {
        let program = "acc = 0; i = 0; while i < 4 { acc = acc + i; i = i + 1; } output acc;";
        let result = run_program(program, BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(6)]))
        );
    }

    #[test]
    fn supports_json_input_vars() {
        let mut input = BTreeMap::new();
        input.insert("base".to_string(), JsonValue::Number(7));
        let result = run_program("total = base + 5; output total;", input).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(12)]))
        );
    }
}
