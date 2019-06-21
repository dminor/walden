use crate::lexer;
use crate::parser;
use crate::vm;

fn generate(ast: &parser::Ast, vm: &mut vm::VirtualMachine, instr: &mut Vec<vm::Opcode>) {
    match ast {
        parser::Ast::Assignment(id, expr) => match &id.token {
            lexer::Token::Identifier(s) => {
                instr.push(vm::Opcode::Srcpos(id.line, id.col));
                generate(expr, vm, instr);
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    s.to_string(),
                )));
                instr.push(vm::Opcode::This);
                instr.push(vm::Opcode::Dup);
                instr.push(vm::Opcode::Const(vm::Value::String(
                    vm.string.clone(),
                    "set:to:".to_string(),
                )));
                instr.push(vm::Opcode::Lookup);
                instr.push(vm::Opcode::Call);
            }
            _ => unreachable!(),
        },
        parser::Ast::Binary(op, lhs, rhs) => {
            generate(lhs, vm, instr);
            generate(rhs, vm, instr);
            instr.push(vm::Opcode::Srcpos(op.line, op.col));
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
                _ => unreachable!(),
            }
        }
        parser::Ast::Block(params, locals, statements) => {
            let mut count = 0;
            let mut block_instr = Vec::new();
            for statement in statements {
                generate(statement, vm, &mut block_instr);
                count += 1;
                if count != statements.len() {
                    block_instr.push(vm::Opcode::Pop);
                }
            }
            block_instr.push(vm::Opcode::Ret);
            let ip = vm.instructions.len();
            vm.instructions.extend(block_instr);
            let mut block_params = Vec::new();
            for param in params {
                match &param.token {
                    lexer::Token::Identifier(id) => {
                        block_params.push(id.to_string());
                    }
                    _ => unreachable!(),
                }
            }
            let mut block_locals = Vec::new();
            for local in locals {
                match &local.token {
                    lexer::Token::Identifier(id) => {
                        block_locals.push(id.to_string());
                    }
                    _ => unreachable!(),
                }
            }

            instr.push(vm::Opcode::Const(vm::Value::Block(
                vm.block.clone(),
                block_params,
                block_locals,
                ip,
            )));
        }
        parser::Ast::Keyword(obj, msg) => {
            let mut message_name = String::new();
            for i in 0..msg.len() {
                instr.push(vm::Opcode::Srcpos(msg[i].0.line, msg[i].0.col));
                message_name.push_str(&msg[i].0.token.to_string());
                message_name.push(':');
                generate(&msg[msg.len() - i - 1].1, vm, instr);
            }
            generate(obj, vm, instr);
            instr.push(vm::Opcode::Dup);
            instr.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                message_name,
            )));
            instr.push(vm::Opcode::Lookup);
            instr.push(vm::Opcode::Call);
        }
        parser::Ast::Lookup(id) => {
            instr.push(vm::Opcode::Srcpos(id.line, id.col));
            instr.push(vm::Opcode::This);
            instr.push(vm::Opcode::Const(vm::Value::String(
                vm.string.clone(),
                id.token.to_string(),
            )));
            instr.push(vm::Opcode::Lookup);
        }
        parser::Ast::Program(statements) => {
            let mut count = 0;
            for statement in statements {
                generate(statement, vm, instr);
                count += 1;
                if count != statements.len() {
                    instr.push(vm::Opcode::Pop);
                }
            }
        }
        parser::Ast::Unary(obj, msg) => {
            generate(obj, vm, instr);
            instr.push(vm::Opcode::Dup);
            instr.push(vm::Opcode::Srcpos(msg.line, msg.col));
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
                instr.push(vm::Opcode::Const(vm::Value::Nil(vm.object.clone())));
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
            _ => unreachable!(),
        },
    }
}

pub fn eval(vm: &mut vm::VirtualMachine, ast: &parser::Ast) -> Result<vm::Value, vm::RuntimeError> {
    let mut instr = Vec::new();
    generate(ast, vm, &mut instr);
    vm.ip = vm.instructions.len();
    vm.instructions.extend(instr);

    match vm.run() {
        Ok(()) => match vm.stack.pop() {
            Some(v) => Ok(v),
            None => {
                return Err(vm::RuntimeError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                    col: usize::max_value(),
                });
            }
        },
        Err(e) => Err(e),
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
        eval!("[1. [2. 3.] value. ] value.", Number, 3.0);
        eval!("true ifTrue: [1.].", Number, 1.0);
        eval!("false ifTrue: [1.].", Nil);
        eval!("false ifFalse: [1.].", Number, 1.0);
        eval!("true ifFalse: [1.].", Nil);
        eval!("false ifTrue: [1.] ifFalse: [2.].", Number, 2.0);
        eval!("true ifTrue: [1.] ifFalse: [2.].", Number, 1.0);
        eval!("[42.] value.", Number, 42.0);
        eval!(
            "[42 prototype set: 'test' to: [true.].
              42 test.] value.",
            Boolean,
            true
        );
        evalfails!("42 test.", "Message not understood.");
        evalfails!(
            "[42 prototype set: 'test' to: true.
              42 test.] value.",
            "Attempt to call non-lambda value."
        );
        eval!(
            "[42 prototype set: 'value' to: [true.].
              42 value.] value.",
            Boolean,
            true
        );
        evalfails!("42 prototype value.", "Message not understood.");

        eval!(
            "[42 prototype set: 'x' to: true.
              42 prototype set: 'getX' to: [@x.].
              42 getX.] value.",
            Boolean,
            true
        );
        eval!(
            "[42 prototype set: 'x' to: 1.
              42 prototype set: 'getX' to: [@x.].
              42 prototype set: 'incX' to: [@x := @x + 1.].
              42 prototype incX incX.
              42 getX.] value.",
            Number,
            3.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'x' with: 1.
             obj override: 'getX' with: [@x.].
             obj override: 'incX' with: [@x := @x + 1.].
             obj incX incX.
             obj getX.",
            Number,
            3.0
        );
        eval!("a := 42. a.", Number, 42.0);
        eval!("a := 42. [[a.] value.] value.", Number, 42.0);
        eval!("a := 1. [[a := 2.] value.] value. a.", Number, 2.0);
        eval!(
            "a := 1.
             b := [a.].
             a := 2.
             b value.",
            Number,
            2.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'test:' with: [:a|a + 1.].
             obj test: 1.",
            Number,
            2.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'x' with: 1.
             obj override: 'test:' with: [:a|@x + a + 1.].
             obj test: 1.",
            Number,
            3.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'test:' with: [:a|[a.].].
             (obj test: 2) value.",
            Number,
            2.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'test:' with: [:a|[a.].].
             f1 := (obj test: 2).
             f2 := (obj test: 3).
             f1 value.",
            Number,
            2.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'test:' with: [:a||y|[y := a. y.]].
             f1 := (obj test: 2).
             f2 := (obj test: 3).
             f1 value.",
            Number,
            2.0
        );

        eval!(
            "obj := Object clone.
             obj override: 'x' with: 1.
             obj override: 'incX' with: [@x := @x + 1].
             obj override: 'test:' with: [:a||y|[y := a. y + @x.]].
             f := (obj test: 2).
             obj incX.
             f value.",
            Number,
            4.0
        );

        eval!(
            "x := 1.
             [x := 2] value.
             x.",
            Number,
            2.0
        );

        eval!(
            "x := 1.
             [x := 2] value.
             [x] value.",
            Number,
            2.0
        );

        eval!(
            "[x := 1] value.
             x := 2.
             [x] value.",
            Number,
            2.0
        );

        eval!(
            "o := Object clone.
             o override: 'test' with: [|
                 | x |
                 x := 1.
                 true ifTrue: [x := 2] ifFalse: [x := 3].
                 x.
             ].
             o test.",
            Number,
            2.0
        );

        eval!(
            "x := 10.
             sum := 0.
             [x > 0] whileTrue: [
                 sum := sum + x.
                 x := x - 1.
             ].
             sum.",
            Number,
            55.0
        );

        eval!("[:x|x + 1] value: 1.", Number, 2.0);
        eval!("[:x :y|x + y] value: 1 value: 2.", Number, 3.0);
        eval!(
            "value prototype set: 'greeting' to: ['hi'].
             value greeting.",
            String,
            "hi"
        );
        eval!(
            "'a string with some words' startsWith: 'a string'.",
            Boolean,
            true
        );
        eval!(
            "('a string with some words' split: ' ') next.",
            String,
            "a".to_string()
        );
    }
}
