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

fn generate(
    ast: &parser::Ast,
    vm: &mut vm::VirtualMachine,
    instr: &mut Vec<vm::Opcode>,
) -> Result<(), RuntimeError> {
    match ast {
        parser::Ast::Assignment(id, expr) => match &id.token {
            lexer::Token::Identifier(s) => {
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
                generate(expr, vm, instr)?;
                instr.push(vm::Opcode::Arg(0));
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    "set:with:".to_string(),
                )));
                instr.push(vm::Opcode::Lookup);
                instr.push(vm::Opcode::Call);
            }
            _ => {
                return Err(RuntimeError {
                    err: "Lookup expected identifier.".to_string(),
                    line: usize::max_value(),
                });
            }
        },
        parser::Ast::Binary(op, lhs, rhs) => {
            generate(lhs, vm, instr)?;
            generate(rhs, vm, instr)?;
            match op.token {
                lexer::Token::Plus => {
                    instr.push(vm::Opcode::Add);
                }
                lexer::Token::Minus => {
                    instr.push(vm::Opcode::Sub);
                }
                lexer::Token::Star => {
                    instr.push(vm::Opcode::Mul);
                }
                lexer::Token::Slash => {
                    instr.push(vm::Opcode::Div);
                }
                lexer::Token::Less => {
                    instr.push(vm::Opcode::Less);
                }
                lexer::Token::LessEqual => {
                    instr.push(vm::Opcode::LessEqual);
                }
                lexer::Token::Equal => {
                    instr.push(vm::Opcode::Equal);
                }
                lexer::Token::NotEqual => {
                    instr.push(vm::Opcode::NotEqual);
                }
                lexer::Token::Greater => {
                    instr.push(vm::Opcode::Greater);
                }
                lexer::Token::GreaterEqual => {
                    instr.push(vm::Opcode::GreaterEqual);
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
            let mut block_instr = Vec::new();
            for statement in statements {
                generate(statement, vm, &mut block_instr)?;
                count += 1;
                if count != statements.len() {
                    block_instr.push(vm::Opcode::Pop);
                }
            }
            block_instr.push(vm::Opcode::Ret);
            let ip = vm.instructions.len();
            vm.instructions.extend(block_instr);
            instr.push(vm::Opcode::Const(vm::Value::Block(vm.object.clone(), ip)));
        }
        parser::Ast::Keyword(obj, msg) => {
            let mut message_name = String::new();
            for kw in msg {
                message_name.push_str(&kw.0.token.to_string());
                message_name.push(':');
                generate(&kw.1, vm, instr)?;
            }
            generate(obj, vm, instr)?;
            instr.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                message_name,
            )));
            instr.push(vm::Opcode::Lookup);
            instr.push(vm::Opcode::Call);
        }
        parser::Ast::Lookup(id) => {
            instr.push(vm::Opcode::Arg(0));
            instr.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                id.token.to_string(),
            )));
            instr.push(vm::Opcode::Lookup);
            instr.push(vm::Opcode::Swap);
            instr.push(vm::Opcode::Pop);
        }
        parser::Ast::Unary(obj, msg) => {
            generate(obj, vm, instr)?;
            instr.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                msg.token.to_string(),
            )));
            instr.push(vm::Opcode::Lookup);
            instr.push(vm::Opcode::Call);
        }
        parser::Ast::Value(v) => match &v.token {
            lexer::Token::False => {
                instr.push(vm::Opcode::Const(vm::Value::Boolean(
                    vm.boolean.clone(),
                    false,
                )));
            }
            lexer::Token::Identifier(s) => {
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
            }
            lexer::Token::Nil => {
                instr.push(vm::Opcode::Const(vm::Value::Nil(vm.nil.clone())));
            }
            lexer::Token::Number(n) => {
                instr.push(vm::Opcode::Const(vm::Value::Number(vm.number.clone(), *n)));
            }
            lexer::Token::String(s) => {
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
            }
            lexer::Token::True => {
                instr.push(vm::Opcode::Const(vm::Value::Boolean(
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

pub fn eval(vm: &mut vm::VirtualMachine, ast: &parser::Ast) -> Result<vm::Value, RuntimeError> {
    let mut instr = Vec::new();
    generate(ast, vm, &mut instr)?;
    vm.ip = vm.instructions.len();
    vm.instructions.extend(instr);

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
        ($input:expr, $type:ident) => {{
            let mut vm = vm::VirtualMachine::new();
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => match interpreter::eval(&mut vm, &ast) {
                        Ok(vm::Value::$type(_)) => {}
                        Err(e) => assert_eq!("Eval failed.", e.err),
                        _ => assert_eq!("Incorrect type.", ""),
                    },
                    _ => assert_eq!("Parse failed.", ""),
                },
                _ => assert_eq!("Scan failed.", ""),
            }
        }};
        ($input:expr, $type:ident, $value:expr) => {{
            let mut vm = vm::VirtualMachine::new();
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => match interpreter::eval(&mut vm, &ast) {
                        Ok(vm::Value::$type(_, v)) => {
                            assert_eq!(v, $value);
                        }
                        Err(e) => assert_eq!("Eval failed.", e.err),
                        _ => assert_eq!("Incorrect type.", ""),
                    },
                    _ => assert_eq!("Parse failed.", ""),
                },
                _ => assert_eq!("Scan failed.", ""),
            }
        }};
    }

    macro_rules! evalfails {
        ($input:expr, $err:tt) => {{
            let mut vm = vm::VirtualMachine::new();
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => match interpreter::eval(&mut vm, &ast) {
                        Err(e) => {
                            assert_eq!(e.err, $err);
                        }
                        _ => assert_eq!("Unexpected success.", ""),
                    },
                    _ => assert_eq!("Parse failed.", ""),
                },
                _ => assert_eq!("Scan failed.", ""),
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
        eval!("[1. 2. 3.].", Block, 0);
        eval!("[1. [2. 3.]. ].", Block, 4);
        eval!("[1. [2. 3.] value. ] value.", Number, 3.0);
        eval!("true ifTrue: [1.].", Number, 1.0);
        eval!("false ifTrue: [1.].", Nil);
        eval!("false ifFalse: [1.].", Number, 1.0);
        eval!("true ifFalse: [1.].", Nil);
        eval!("false ifTrue: [1.] ifFalse: [2.].", Number, 2.0);
        eval!("true ifTrue: [1.] ifFalse: [2.].", Number, 1.0);
        eval!("[42.] value.", Number, 42.0);
        eval!(
            "[42 prototype set: 'test' with: [true.].
              42 test.] value.",
            Boolean,
            true
        );
        evalfails!("42 test.", "Message not understood.");
        evalfails!(
            "[42 prototype set: 'test' with: true.
              42 test.] value.",
            "Attempt to call non-lambda value."
        );
        eval!("42 value.", Number, 42.0);
        eval!(
            "[42 prototype set: 'value' with: [true.].
              42 value.] value.",
            Boolean,
            true
        );
        evalfails!("42 prototype value.", "Message not understood.");
        eval!(
            "[42 prototype set: 'x' with: true.
              42 prototype set: 'getX' with: [x.].
              42 getX.] value.",
            Boolean,
            true
        );
        eval!(
            "[42 prototype set: 'x' with: 1.
              42 prototype set: 'getX' with: [x.].
              42 prototype set: 'incX' with: [x := x + 1.].
              42 prototype incX incX.
              42 getX.] value.",
            Number,
            3.0
        );
    }
}
