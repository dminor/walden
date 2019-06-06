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

fn generate(ast: &parser::Ast, vm: &mut vm::VirtualMachine) -> Result<(), RuntimeError> {
    match ast {
        parser::Ast::Binary(op, lhs, rhs) => {
            generate(lhs, vm)?;
            generate(rhs, vm)?;
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
                lexer::Token::Less => {
                    vm.instructions.push(vm::Opcode::Less);
                }
                lexer::Token::LessEqual => {
                    vm.instructions.push(vm::Opcode::LessEqual);
                }
                lexer::Token::Equal => {
                    vm.instructions.push(vm::Opcode::Equal);
                }
                lexer::Token::NotEqual => {
                    vm.instructions.push(vm::Opcode::NotEqual);
                }
                lexer::Token::Greater => {
                    vm.instructions.push(vm::Opcode::Greater);
                }
                lexer::Token::GreaterEqual => {
                    vm.instructions.push(vm::Opcode::GreaterEqual);
                }
                _ => {
                    return Err(RuntimeError {
                        err: "Unsupported operation.".to_string(),
                        line: usize::max_value(),
                    });
                }
            }
        }
        parser::Ast::Block(_, _, statements) => {
            let mut count = 0;
            for statement in statements {
                generate(statement, vm)?;
                count += 1;
                if count != statements.len() {
                    vm.instructions.push(vm::Opcode::Pop);
                }
            }
        }
        parser::Ast::Keyword(obj, msg) => {
            let mut message_name = String::new();
            for kw in msg {
                message_name.push_str(&kw.0.token.to_string());
                message_name.push(':');
                generate(&kw.1, vm)?;
            }
            generate(obj, vm)?;
            vm.instructions.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                message_name,
            )));
            vm.instructions.push(vm::Opcode::Lookup);
            vm.instructions.push(vm::Opcode::Call);
        }
        parser::Ast::Unary(obj, msg) => {
            generate(obj, vm)?;
            generate(msg, vm)?;
            vm.instructions.push(vm::Opcode::Lookup);
            vm.instructions.push(vm::Opcode::Call);
        }
        parser::Ast::Value(v) => match &v.token {
            lexer::Token::False => {
                vm.instructions.push(vm::Opcode::Const(vm::Value::Boolean(
                    vm.boolean.clone(),
                    false,
                )));
            }
            lexer::Token::Identifier(s) => {
                vm.instructions.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
            }
            lexer::Token::Nil => {
                vm.instructions
                    .push(vm::Opcode::Const(vm::Value::Nil(vm.nil.clone())));
            }
            lexer::Token::Number(n) => {
                vm.instructions
                    .push(vm::Opcode::Const(vm::Value::Number(vm.number.clone(), *n)));
            }
            lexer::Token::String(s) => {
                vm.instructions.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
            }
            lexer::Token::True => {
                vm.instructions.push(vm::Opcode::Const(vm::Value::Boolean(
                    vm.boolean.clone(),
                    true,
                )));
            }
            _ => {
                return Err(RuntimeError {
                    err: "Unsupported value.".to_string(),
                    line: usize::max_value(),
                });
            }
        },
    }
    Ok(())
}

pub fn eval(ast: &parser::Ast, show_instr: bool) -> Result<vm::Value, RuntimeError> {
    let mut vm = vm::VirtualMachine::new();
    generate(ast, &mut vm)?;

    if show_instr {
        for instr in &vm.instructions {
            println!("    {}", instr);
        }
    }

    match vm.run() {
        Ok(()) => match vm.stack.pop() {
            Some(v) => Ok(v),
            None => {
                return Err(RuntimeError {
                    err: "Stack underflow.".to_string(),
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
    use crate::vm;

    macro_rules! eval {
        ($input:expr, $type:ident, $value:expr) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => match interpreter::eval(&ast, false) {
                        Ok(vm::Value::$type(_, v)) => {
                            assert_eq!(v, $value);
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
        eval!("42.", Number, 42.0);
        eval!("1 + 2.", Number, 3.0);
        eval!("1 - 2.", Number, -1.0);
        eval!("1 - -2.", Number, 3.0);
        eval!("3 * -2.", Number, -6.0);
        eval!("1 / 2.", Number, 0.5);
        eval!("1 + 2 + 3.", Number, 6.0);
        eval!("true.", Boolean, true);
        eval!("false.", Boolean, false);
        eval!("1 <= 3.", Boolean, true);
        eval!("1 < 3 = true.", Boolean, true);
        eval!("1 < 3 ~= false.", Boolean, true);
        eval!("2 + 4 = 7 = false.", Boolean, true);
        eval!("'hello world'.", String, "hello world");
        eval!("true not.", Boolean, false);
        eval!("(3 < 2) not.", Boolean, true);
        eval!("(2 < 3) and: (1 < 3).", Boolean, true);
        eval!("(3 < 2) and: (1 < 3).", Boolean, false);
        eval!("(3 < 2) or: (1 < 3).", Boolean, true);
        eval!("[1. 2. 3.].", Number, 3.0);
    }
}
