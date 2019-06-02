use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct VMError {
    pub err: String,
    pub line: usize,
}

impl fmt::Display for VMError {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "VMError: {}", self.err)
    }
}

impl Error for VMError {}

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

pub enum Opcode {
    Add,
    Div,
    Const(Value),
    Mul,
    Sub,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
}

impl fmt::Display for Opcode {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::Add => write!(f, "add"),
            Opcode::Div => write!(f, "div"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Const(obj) => write!(f, "const {}", obj),
            Opcode::Sub => write!(f, "sub"),
            Opcode::Less => write!(f, "lt"),
            Opcode::LessEqual => write!(f, "le"),
            Opcode::Equal => write!(f, "eq"),
            Opcode::NotEqual => write!(f, "ne"),
            Opcode::Greater => write!(f, "gt"),
            Opcode::GreaterEqual => write!(f, "ge"),
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
                    return Err(VMError {
                        err: err,
                        line: usize::max_value(),
                    });
                }
                None => {
                    return Err(VMError {
                        err: "Stack underflow.".to_string(),
                        line: usize::max_value(),
                    });
                }
            },
            Some(_) => {
                let mut err = "Unsupported types for ".to_string();
                err.push_str(&Opcode::$opcode.to_string());
                err.push('.');
                return Err(VMError {
                    err: err,
                    line: usize::max_value(),
                });
            }
            None => {
                return Err(VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                });
            }
        }
    );
}

impl VirtualMachine {
    pub fn assemble(&mut self, code: &str) -> Result<(), VMError> {
        let mut lineno = 1;
        for line in code.split('\n') {
            let split: Vec<&str> = line.trim().split_whitespace().collect();
            if split.is_empty() {
                lineno += 1;
                continue;
            }

            if let Some(';') = split[0].chars().next() {
                lineno += 1;
                continue;
            }

            match split[0] {
                "const" => {
                    if split.len() == 2 {
                        if split[1] == "true" {
                            self.instructions.push(Opcode::Const(Value::Boolean(true)));
                        } else if split[1] == "false" {
                            self.instructions.push(Opcode::Const(Value::Boolean(false)));
                        } else {
                            if let Some('\'') = split[1].chars().next() {
                                self.instructions
                                    .push(Opcode::Const(Value::String(split[1][1..].to_string())));
                            } else {
                                match split[1].parse::<f64>() {
                                    Ok(n) => {
                                        self.instructions.push(Opcode::Const(Value::Number(n)));
                                    }
                                    _ => {
                                        return Err(VMError {
                                            err: "Invalid constant.".to_string(),
                                            line: lineno,
                                        });
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(VMError {
                            err: "Missing constant argument.".to_string(),
                            line: lineno,
                        });
                    }
                }
                "add" => {
                    self.instructions.push(Opcode::Add);
                }
                "div" => {
                    self.instructions.push(Opcode::Div);
                }
                "mul" => {
                    self.instructions.push(Opcode::Mul);
                }
                "sub" => {
                    self.instructions.push(Opcode::Sub);
                }
                "lt" => {
                    self.instructions.push(Opcode::Less);
                }
                "le" => {
                    self.instructions.push(Opcode::LessEqual);
                }
                "eq" => {
                    self.instructions.push(Opcode::Equal);
                }
                "ne" => {
                    self.instructions.push(Opcode::NotEqual);
                }
                "gt" => {
                    self.instructions.push(Opcode::Greater);
                }
                "ge" => {
                    self.instructions.push(Opcode::GreaterEqual);
                }
                _ => {
                    return Err(VMError {
                        err: "Invalid instruction.".to_string(),
                        line: lineno,
                    });
                }
            }
            lineno += 1;
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), VMError> {
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
                    Value::String(s) => {
                        self.stack.push(Value::String(s.to_string()));
                    }
                },
                Opcode::Div => apply_op!(self, Number, Number, /, Div),
                Opcode::Mul => apply_op!(self, Number, Number, *, Mul),
                Opcode::Sub => apply_op!(self, Number, Number, -, Sub),
                Opcode::Less => apply_op!(self, Number, Boolean, <, Less),
                Opcode::LessEqual => apply_op!(self, Number, Boolean, <=, LessEqual),
                Opcode::Equal => match self.stack.last() {
                    Some(Value::Boolean(_)) => apply_op!(self, Boolean, Boolean, ==, Equal),
                    Some(Value::Number(_)) => apply_op!(self, Number, Boolean, ==, Equal),
                    Some(Value::String(_)) => apply_op!(self, String, Boolean, ==, Equal),
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::NotEqual => match self.stack.last() {
                    Some(Value::Boolean(_)) => apply_op!(self, Boolean, Boolean, !=, NotEqual),
                    Some(Value::Number(_)) => apply_op!(self, Number, Boolean, !=, NotEqual),
                    Some(Value::String(_)) => apply_op!(self, String, Boolean, !=, NotEqual),
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Greater => apply_op!(self, Number, Boolean, >, Greater),
                Opcode::GreaterEqual => apply_op!(self, Number, Boolean, >=, GreaterEqual),
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

    macro_rules! run {
        ($input:expr, $stack:expr, $ip:expr, $type:ident, $value:expr) => {{
            let mut vm = vm::VirtualMachine::new();
            match vm.assemble($input) {
                Ok(()) => match vm.run() {
                    Ok(()) => {
                        assert_eq!(vm.stack.len(), $stack);
                        assert_eq!(vm.ip, $ip);
                        match vm.stack.pop() {
                            Some(vm::Value::$type(v)) => {
                                assert_eq!(v, $value);
                            }
                            _ => {
                                assert!(false);
                            }
                        }
                    }
                    _ => {
                        assert!(false);
                    }
                },
                _ => {
                    assert!(false);
                }
            }
        }};
    }

    macro_rules! runfails {
        ($input:expr, $err:tt) => {{
            let mut vm = vm::VirtualMachine::new();
            match vm.assemble($input) {
                Ok(()) => match vm.run() {
                    Ok(()) => {
                        assert!(false);
                    }
                    Err(e) => {
                        assert_eq!(e.err, $err);
                    }
                },
                Err(_) => {
                    assert!(false);
                }
            }
        }};
    }

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

        run!(
            "
            const 1.0
            const 2.0
            add
        ",
            1,
            3,
            Number,
            3.0
        );

        run!(
            "
            const 1.0
            const 2.0
            div
        ",
            1,
            3,
            Number,
            0.5
        );

        run!(
            "
            const 1.0
            const 2.0
            mul
        ",
            1,
            3,
            Number,
            2.0
        );

        run!(
            "
            const 1.0
            const 2.0
            sub
        ",
            1,
            3,
            Number,
            -1.0
        );

        runfails!(
            "
            add
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const 1.0
            add
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const false
            const true
            add
        ",
            "Unsupported types for add."
        );

        runfails!(
            "
            const true
            const 1.0
            add
        ",
            "Unsupported types for add."
        );

        run!(
            "
            const 1.0
            const 2.0
            lt
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 2.0
            le
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            eq
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
        ",
            1,
            3,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            gt
        ",
            1,
            3,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const true
            const false
            eq
        ",
            1,
            3,
            Boolean,
            false
        );

        run!(
            "
            const -1.0
            const 2.0
            lt
            const false
            eq
        ",
            1,
            5,
            Boolean,
            false
        );

        run!(
            "
            const 'hello
            const 'hello
            eq
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const '
            const '
            eq
        ",
            1,
            3,
            Boolean,
            true
        );

        run!(
            "
            const 'hello
            const 'world
            eq
        ",
            1,
            3,
            Boolean,
            false
        );
    }
}
