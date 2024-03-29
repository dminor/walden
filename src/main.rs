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

fn eval(filename: &str, src: &str, vm: &mut vm::VirtualMachine) {
    let lines: Vec<&str> = src.split('\n').collect();
    match lexer::scan(src) {
        Ok(mut tokens) => match parser::parse(&mut tokens) {
            Ok(ast) => match interpreter::eval(vm, &ast) {
                Ok(v) => {
                    println!("{}", v);
                }
                Err(err) => {
                    let line = min(lines.len(), err.line);
                    let col = min(lines[line - 1].len(), err.col);
                    let width = line.to_string().len() + 2;
                    println!("{}", err);
                    println!("{s:>width$}|", s = " ", width = width);
                    println!(" {} | {}", line, lines[line - 1]);
                    print!("{s:>width$}|", s = " ", width = width);
                    println!("{s:>width$}^", s = " ", width = col);
                    println!("--> {}:{}", filename, line);
                    vm.stack.drain(0..);
                    vm.callstack.drain(0..);
                }
            },
            Err(err) => {
                let line = min(lines.len(), err.line);
                let col = min(lines[line - 1].len(), err.col);
                let width = line.to_string().len() + 2;
                println!("{}", err);
                println!("{s:>width$}|", s = " ", width = width);
                println!(" {} | {}", line, lines[line - 1]);
                print!("{s:>width$}|", s = " ", width = width);
                println!("{s:>width$}^", s = " ", width = col);
                println!("--> {}:{}", filename, line);
            }
        },
        Err(err) => {
            let line = min(lines.len(), err.line);
            let col = min(lines[line - 1].len(), err.col);
            let width = line.to_string().len() + 2;
            println!("{}", err);
            println!("{s:>width$}|", s = " ", width = width);
            println!(" {} | {}", line, lines[line - 1]);
            print!("{s:>width$}|", s = " ", width = width);
            println!("{s:>width$}^", s = " ", width = col);
            println!("--> {}:{}", filename, line);
        }
    }
}

fn main() -> io::Result<()> {
    let mut vm = vm::VirtualMachine::new();
    let args: Vec<String> = env::args().collect();
    for i in 1..args.len() {
        if args[i] == "--enable-tracing" {
            vm.enable_tracing = true;
        } else {
            let filename = &args[i];
            let mut file = File::open(filename)?;
            let mut program = String::new();
            file.read_to_string(&mut program)?;
            eval(&filename, &program, &mut vm);
            return Ok(());
        }
    }

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    println!("Welcome to Walden!");
    print!("> ");
    stdout.flush()?;

    for line in stdin.lock().lines() {
        match line {
            Ok(src) => {
                eval("<stdin>", &src, &mut vm);
            }
            _ => break,
        }
        print!("> ");
        stdout.flush()?;
    }

    Ok(())
}
