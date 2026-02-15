use std::io::{self, Read};

mod ast;
mod dsl_error;
mod interpreter;
mod json;
mod lexer;
mod parser;
mod runtime;

use runtime::{error_json, parse_payload, run_program};

fn main() {
    let mut buf = String::new();
    let exit = match io::stdin().read_to_string(&mut buf) {
        Ok(_) => match parse_payload(&buf) {
            Ok((program, vars)) => match run_program(&program, vars) {
                Ok(out) => {
                    println!("{}", out.to_json());
                    0
                }
                Err(e) => {
                    println!("{}", error_json(&e.to_string()));
                    1
                }
            },
            Err(e) => {
                println!("{}", error_json(&e.to_string()));
                1
            }
        },
        Err(e) => {
            println!("{}", error_json(&format!("Failed reading stdin: {e}")));
            1
        }
    };
    std::process::exit(exit);
}
