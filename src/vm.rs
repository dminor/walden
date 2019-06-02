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

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
        }
    }
}

pub enum Opcode {
    Add,
    Div,
    Const(Value),
    Mul,
    Sub,
}

impl fmt::Display for Opcode {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::Add => write!(f, "add"),
            Opcode::Div => write!(f, "div"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Const(obj) => write!(f, "const {}", obj),
            Opcode::Sub => write!(f, "sub"),
        }
    }
}

#[derive(Default)]
pub struct VirtualMachine {
    pub stack: Vec<Value>,
    pub instructions: Vec<Opcode>,
    pub ip: usize,
}

macro_rules! apply_op {
    ($self:tt, $in:tt, $out:tt, $rustop:tt, $opcode:tt) => (
        match $self.stack.pop() {
            Some(Value::$in(a)) => match $self.stack.pop() {
                Some(Value::$in(b)) => {
                    $self.stack.push(Value::$out(b $rustop a));
                }
                Some(_) => {
                    let mut err = "Unsupported types for ".to_string();
                    err.push_str(&Opcode::$opcode.to_string());
                    err.push('.');
                    return Err(RuntimeError {
                        err: err,
                        line: usize::max_value(),
                    });
                }
                None => {
                    return Err(RuntimeError {
                        err: "Stack underflow.".to_string(),
                        line: usize::max_value(),
                    });
                }
            },
            Some(_) => {
                let mut err = "Unsupported types for ".to_string();
                err.push_str(&Opcode::$opcode.to_string());
                err.push('.');
                return Err(RuntimeError {
                    err: err,
                    line: usize::max_value(),
                });
            }
            None => {
                return Err(RuntimeError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                });
            }
        }
    );
}

impl VirtualMachine {
    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.ip = 0;
        while self.ip < self.instructions.len() {
            match &self.instructions[self.ip] {
                Opcode::Add => apply_op!(self, Number, Number, +, Add),
                Opcode::Const(obj) => match obj {
                    Value::Boolean(b) => {
                        self.stack.push(Value::Boolean(*b));
                    }
                    Value::Number(n) => {
                        self.stack.push(Value::Number(*n));
                    }
                },
                Opcode::Div => apply_op!(self, Number, Number, /, Div),
                Opcode::Mul => apply_op!(self, Number, Number, *, Mul),
                Opcode::Sub => apply_op!(self, Number, Number, -, Sub),
            }
            self.ip += 1;
        }
        Ok(())
    }

    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: Vec::new(),
            instructions: Vec::new(),
            ip: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm;

    #[test]
    fn run() {
        let mut vm = vm::VirtualMachine::new();
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 0);
                assert_eq!(vm.ip, 0);
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(1.0)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(2.0)));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(vm::Value::Number(n)) => {
                        assert_eq!(n, 3.0);
                    }
                    _ => {
                        assert!(false);
                    }
                }
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(1.0)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(2.0)));
        vm.instructions.push(vm::Opcode::Div);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(vm::Value::Number(n)) => {
                        assert_eq!(n, 0.5);
                    }
                    _ => {
                        assert!(false);
                    }
                }
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(1.0)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(2.0)));
        vm.instructions.push(vm::Opcode::Mul);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(vm::Value::Number(n)) => {
                        assert_eq!(n, 2.0);
                    }
                    _ => {
                        assert!(false);
                    }
                }
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(1.0)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(2.0)));
        vm.instructions.push(vm::Opcode::Sub);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(vm::Value::Number(n)) => {
                        assert_eq!(n, -1.0);
                    }
                    _ => {
                        assert!(false);
                    }
                }
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert!(false);
            }
            Err(e) => {
                assert_eq!(e.err, "Stack underflow.");
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(3.0)));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert!(false);
            }
            Err(e) => {
                assert_eq!(e.err, "Stack underflow.");
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Boolean(false)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Boolean(true)));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert!(false);
            }
            Err(e) => {
                assert_eq!(e.err, "Unsupported types for add.");
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Number(3.0)));
        vm.instructions
            .push(vm::Opcode::Const(vm::Value::Boolean(true)));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert!(false);
            }
            Err(e) => {
                assert_eq!(e.err, "Unsupported types for add.");
            }
        }
    }
}
