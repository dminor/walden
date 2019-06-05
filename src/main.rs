use std::cmp::min;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod interpreter;
mod lexer;
mod parser;
mod stdlib;
mod vm;

use std::io::{self, BufRead, Write};

fn eval(filename: String, s: String) {
    let lines: Vec<&str> = s.split('\n').collect();
    match lexer::scan(&s) {
        Ok(mut tokens) => match parser::parse(&mut tokens) {
            Ok(ast) => match interpreter::eval(&ast, true) {
                Ok(v) => {
                    println!("{}", v);
                }
                Err(err) => {
                    let line = min(lines.len(), err.line);
                    let width = line.to_string().len() + 2;
                    println!("{}", err);
                    println!("{s:>width$}|", s = " ", width = width);
                    println!(" {} | {}", line, lines[line - 1]);
                    println!("{s:>width$}|", s = " ", width = width);
                    println!("--> {}:{}", filename, line);
                }
            },
            Err(err) => {
                let line = min(lines.len(), err.line);
                let width = line.to_string().len() + 2;
                println!("{}", err);
                println!("{s:>width$}|", s = " ", width = width);
                println!(" {} | {}", line, lines[line - 1]);
                println!("{s:>width$}|", s = " ", width = width);
                println!("--> {}:{}", filename, line);
            }
        },
        Err(err) => {
            let line = min(lines.len(), err.line);
            let width = line.to_string().len() + 2;
            println!("{}", err);
            println!("{s:>width$}|", s = " ", width = width);
            println!(" {} | {}", line, lines[line - 1]);
            println!("{s:>width$}|", s = " ", width = width);
            println!("--> {}:{}", filename, line);
        }
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let filename = args[1].to_string();
        let mut file = File::open(&filename)?;
        let mut program = String::new();
        file.read_to_string(&mut program)?;
        eval(filename, program);
        return Ok(());
    }

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    println!("Welcome to Walden!");
    print!("> ");
    stdout.flush()?;

    for line in stdin.lock().lines() {
        match line {
            Ok(s) => {
                eval("<stdin>".to_string(), s);
            }
            _ => break,
        }
        print!("> ");
        stdout.flush()?;
    }

    Ok(())
}
