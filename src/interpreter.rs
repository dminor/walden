use crate::lexer;
use crate::parser;
use crate::vm;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct RuntimeError {
    pub err: String,
    pub line: usize,
}

impl fmt::Display for RuntimeError {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.err)
    }
}

impl Error for RuntimeError {}

fn assemble(ast: &parser::Ast, vm: &mut vm::VirtualMachine) -> Result<(), RuntimeError> {
    match ast {
        parser::Ast::Binary(op, lhs, rhs) => {
            assemble(lhs, vm)?;
            assemble(rhs, vm)?;
            match op.token {
                lexer::Token::Plus => {
                    vm.instructions.push(vm::Opcode::Add);
                }
                lexer::Token::Minus => {
                    vm.instructions.push(vm::Opcode::Sub);
                }
                lexer::Token::Star => {
                    vm.instructions.push(vm::Opcode::Mul);
                }
                lexer::Token::Slash => {
                    vm.instructions.push(vm::Opcode::Div);
                }
                _ => {
                    return Err(RuntimeError {
                        err: "Unsupported operation.".to_string(),
                        line: usize::max_value(),
                    });
                }
            }
        }
        parser::Ast::Value(v) => match v.token {
            lexer::Token::Number(n) => {
                vm.fconsts.push(n);
                vm.instructions
                    .push(vm::Opcode::ConstF(vm.fconsts.len() - 1));
            }
            _ => {
                return Err(RuntimeError {
                    err: "Unsupported value.".to_string(),
                    line: usize::max_value(),
                });
            }
        },
        _ => {
            return Err(RuntimeError {
                err: "Unimplemented.".to_string(),
                line: usize::max_value(),
            });
        }
    }
    Ok(())
}

pub fn eval(ast: &parser::Ast) -> Result<f64, RuntimeError> {
    let mut vm = vm::VirtualMachine::new();
    assemble(ast, &mut vm)?;
    match vm.run() {
        Ok(()) => match vm.stack.pop() {
            Some(n) => Ok(n),
            None => {
                return Err(RuntimeError {
                    err: "vm error.".to_string(),
                    line: usize::max_value(),
                });
            }
        },
        Err(e) => {
            return Err(RuntimeError {
                err: e.err,
                line: usize::max_value(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter;
    use crate::lexer;
    use crate::parser;

    macro_rules! eval {
        ($input:expr, $value:expr) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => match interpreter::eval(&ast) {
                        Ok(n) => {
                            assert_eq!(n, $value);
                        }
                        _ => assert!(false),
                    },
                    _ => assert!(false),
                },
                _ => assert!(false),
            }
        }};
    }

    #[test]
    fn evals() {
        eval!("42", 42.0);
        eval!("1 + 2", 3.0);
        eval!("1 - 2", -1.0);
        eval!("1 - -2", 3.0);
        eval!("3 * -2", -6.0);
        eval!("1 / 2", 0.5);
        eval!("1 + 2 + 3", 6.0);
    }
}
