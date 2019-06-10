use crate::stdlib;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

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

#[derive(Default)]
pub struct Object {
    pub prototype: Option<Rc<RefCell<Object>>>,
    pub members: HashMap<String, Value>,
}

impl Object {
    pub fn lookup(&self, member: String) -> Option<Value> {
        match self.members.get(&member) {
            Some(v) => match v {
                Value::Block(proto, address) => Some(Value::Block(proto.clone(), *address)),
                Value::Boolean(proto, b) => Some(Value::Boolean(proto.clone(), *b)),
                Value::Nil(proto) => Some(Value::Nil(proto.clone())),
                Value::Number(proto, n) => Some(Value::Number(proto.clone(), *n)),
                Value::Object(obj) => Some(Value::Object(obj.clone())),
                Value::RustBlock(f) => Some(Value::RustBlock(*f)),
                Value::String(proto, s) => Some(Value::String(proto.clone(), s.to_string())),
            },
            None => match &self.prototype {
                Some(proto) => proto.borrow().lookup(member),
                None => None,
            },
        }
    }

    pub fn new() -> Object {
        Object {
            prototype: None,
            members: HashMap::new(),
        }
    }

    pub fn new_with_prototype(proto: Rc<RefCell<Object>>) -> Object {
        Object {
            prototype: Some(proto.clone()),
            members: HashMap::new(),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Block(Rc<RefCell<Object>>, usize),
    Boolean(Rc<RefCell<Object>>, bool),
    Nil(Rc<RefCell<Object>>),
    Number(Rc<RefCell<Object>>, f64),
    Object(Rc<RefCell<Object>>),
    RustBlock(fn(&mut VirtualMachine) -> Result<(), VMError>),
    String(Rc<RefCell<Object>>, String),
}

impl fmt::Display for Value {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Block(_, u) => write!(f, "(lambda @{})", u),
            Value::Boolean(_, b) => write!(f, "{}", b),
            Value::Nil(_) => write!(f, "nil"),
            Value::Number(_, n) => write!(f, "{}", n),
            Value::Object(_) => write!(f, "(object)"),
            Value::RustBlock(_) => write!(f, "(lambda)"),
            Value::String(_, s) => write!(f, "'{}'", s),
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
    Arg(usize),
    Call,
    Lookup,
    Pop,
    Ret,
    Swap,
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
            Opcode::Arg(i) => write!(f, "arg {}", i),
            Opcode::Call => write!(f, "call"),
            Opcode::Lookup => write!(f, "lookup"),
            Opcode::Pop => write!(f, "pop"),
            Opcode::Ret => write!(f, "ret"),
            Opcode::Swap => write!(f, "swap"),
        }
    }
}

#[derive(Default)]
pub struct VirtualMachine {
    pub callstack: Vec<(usize, usize)>,
    pub instructions: Vec<Opcode>,
    pub ip: usize,
    pub stack: Vec<Value>,

    // Prototypes for builtin objects
    pub object: Rc<RefCell<Object>>,
    pub boolean: Rc<RefCell<Object>>,
    pub nil: Rc<RefCell<Object>>,
    pub number: Rc<RefCell<Object>>,
    pub string: Rc<RefCell<Object>>,

    pub enable_tracing: bool,
}

macro_rules! apply_op {
    ($self:tt, $in:tt, $out:tt, $proto:expr, $rustop:tt, $opcode:tt) => (
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self.stack.push(Value::$out($proto, b $rustop a));
                }
                None => {
                    return Err(VMError {
                        err: "Stack underflow.".to_string(),
                        line: usize::max_value(),
                    });
                }
                _ => {
                    let mut err = "Unsupported types for ".to_string();
                    err.push_str(&Opcode::$opcode.to_string());
                    err.push('.');
                    return Err(VMError {
                        err: err,
                        line: usize::max_value(),
                    });
                }
            },
            None => {
                return Err(VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                });
            }
            _ => {
                let mut err = "Unsupported types for ".to_string();
                err.push_str(&Opcode::$opcode.to_string());
                err.push('.');
                return Err(VMError {
                    err: err,
                    line: usize::max_value(),
                });
            }
        }
    );
}

macro_rules! apply_eq {
    ($self:tt, $in:tt) => {
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), b == a));
                }
                None => {
                    return Err(VMError {
                        err: "Stack underflow.".to_string(),
                        line: usize::max_value(),
                    });
                }
                _ => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), false));
                }
            },
            None => {
                return Err(VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                });
            }
            _ => unreachable!(),
        }
    };
}

macro_rules! apply_neq {
    ($self:tt, $in:tt) => {
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), b != a));
                }
                None => {
                    return Err(VMError {
                        err: "Stack underflow.".to_string(),
                        line: usize::max_value(),
                    });
                }
                _ => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), true));
                }
            },
            None => {
                return Err(VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                });
            }
            _ => unreachable!(),
        }
    };
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
                    if split.len() >= 2 {
                        if split[1] == "false" {
                            self.instructions
                                .push(Opcode::Const(Value::Boolean(self.boolean.clone(), false)));
                        } else if split[1] == "nil" {
                            self.instructions
                                .push(Opcode::Const(Value::Nil(self.nil.clone())));
                        } else if split[1] == "true" {
                            self.instructions
                                .push(Opcode::Const(Value::Boolean(self.boolean.clone(), true)));
                        } else {
                            if let Some('\'') = split[1].chars().next() {
                                let mut string = split[1][1..].to_string();
                                for s in &split[2..] {
                                    string.push(' ');
                                    string.push_str(s);
                                }

                                self.instructions.push(Opcode::Const(Value::String(
                                    self.string.clone(),
                                    string,
                                )));
                            } else {
                                match split[1].parse::<f64>() {
                                    Ok(n) => {
                                        self.instructions.push(Opcode::Const(Value::Number(
                                            self.number.clone(),
                                            n,
                                        )));
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
                "arg" => {
                    if split.len() >= 2 {
                        match split[1].parse::<usize>() {
                            Ok(i) => self.instructions.push(Opcode::Arg(i)),
                            Err(_) => {
                                return Err(VMError {
                                    err: "arg requires index.".to_string(),
                                    line: lineno,
                                });
                            }
                        }
                    } else {
                        return Err(VMError {
                            err: "arg requires index.".to_string(),
                            line: lineno,
                        });
                    }
                }
                "block" => {
                    if split.len() >= 2 {
                        match split[1].parse::<usize>() {
                            Ok(ip) => {
                                self.instructions
                                    .push(Opcode::Const(Value::Block(self.object.clone(), ip)));
                            }
                            Err(_) => {
                                return Err(VMError {
                                    err: "block requires ip.".to_string(),
                                    line: lineno,
                                });
                            }
                        }
                    } else {
                        return Err(VMError {
                            err: "block requires ip.".to_string(),
                            line: lineno,
                        });
                    }
                }
                "call" => {
                    self.instructions.push(Opcode::Call);
                }
                "lookup" => {
                    self.instructions.push(Opcode::Lookup);
                }
                "pop" => {
                    self.instructions.push(Opcode::Pop);
                }
                "ret" => {
                    self.instructions.push(Opcode::Ret);
                }
                "swap" => {
                    self.instructions.push(Opcode::Swap);
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
        while self.ip < self.instructions.len() {
            if self.enable_tracing {
                println!("{}: {}", self.ip, self.instructions[self.ip]);
            }
            match &self.instructions[self.ip] {
                Opcode::Add => apply_op!(self, Number, Number, self.number.clone(), +, Add),
                Opcode::Const(obj) => match obj {
                    Value::Block(proto, ip) => {
                        self.stack.push(Value::Block(proto.clone(), *ip));
                    }
                    Value::Boolean(proto, b) => {
                        self.stack.push(Value::Boolean(proto.clone(), *b));
                    }
                    Value::Nil(proto) => {
                        self.stack.push(Value::Nil(proto.clone()));
                    }
                    Value::Number(proto, n) => {
                        self.stack.push(Value::Number(proto.clone(), *n));
                    }
                    Value::Object(_) => {
                        unreachable!();
                    }
                    Value::RustBlock(_) => {
                        unreachable!();
                    }
                    Value::String(proto, s) => {
                        self.stack.push(Value::String(proto.clone(), s.to_string()));
                    }
                },
                Opcode::Div => apply_op!(self, Number, Number, self.number.clone(),  /, Div),
                Opcode::Mul => apply_op!(self, Number, Number, self.number.clone(),  *, Mul),
                Opcode::Sub => apply_op!(self, Number, Number, self.number.clone(),  -, Sub),
                Opcode::Less => apply_op!(self, Number, Boolean, self.boolean.clone(),  <, Less),
                Opcode::LessEqual => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), <=, LessEqual)
                }
                Opcode::Equal => match self.stack.last() {
                    Some(Value::Block(_, _)) => apply_eq!(self, Boolean),
                    Some(Value::Boolean(_, _)) => apply_eq!(self, Boolean),
                    Some(Value::Number(_, _)) => apply_eq!(self, Number),
                    Some(Value::Object(_)) => match self.stack.pop() {
                        Some(Value::Object(a)) => match self.stack.pop() {
                            Some(Value::Object(b)) => {
                                self.stack
                                    .push(Value::Boolean(self.boolean.clone(), Rc::ptr_eq(&a, &b)));
                            }
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::Nil(_)) => match self.stack.pop() {
                        Some(Value::Nil(_)) => match self.stack.pop() {
                            Some(Value::Nil(_)) => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => unreachable!(),
                    },

                    Some(Value::RustBlock(_)) => match self.stack.pop() {
                        Some(Value::RustBlock(_)) => match self.stack.pop() {
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::String(_, _)) => apply_eq!(self, String),
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::NotEqual => match self.stack.last() {
                    Some(Value::Block(_, _)) => apply_neq!(self, Boolean),
                    Some(Value::Boolean(_, _)) => apply_neq!(self, Boolean),
                    Some(Value::Number(_, _)) => apply_neq!(self, Number),
                    Some(Value::Object(_)) => match self.stack.pop() {
                        Some(Value::Object(a)) => match self.stack.pop() {
                            Some(Value::Object(b)) => {
                                self.stack.push(Value::Boolean(
                                    self.boolean.clone(),
                                    !Rc::ptr_eq(&a, &b),
                                ));
                            }
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::Nil(_)) => match self.stack.pop() {
                        Some(Value::Nil(_)) => match self.stack.pop() {
                            Some(Value::Nil(_)) => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => {
                            self.stack.push(Value::Boolean(self.boolean.clone(), true));
                        }
                    },
                    Some(Value::RustBlock(_)) => match self.stack.pop() {
                        Some(Value::RustBlock(_)) => match self.stack.pop() {
                            None => {
                                return Err(VMError {
                                    err: "Stack underflow.".to_string(),
                                    line: usize::max_value(),
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                        },
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::String(_, _)) => apply_neq!(self, String),
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Greater => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), >, Greater)
                }
                Opcode::GreaterEqual => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), >=, GreaterEqual)
                }
                Opcode::Arg(u) => match self.callstack.last() {
                    Some((sp, _)) => {
                        self.stack.push(self.stack[sp + u - 1].clone());
                    }
                    None => {
                        return Err(VMError {
                            err: "Call stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Call => match self.stack.pop() {
                    Some(Value::Block(_, ip)) => {
                        self.callstack.push((self.stack.len(), self.ip + 1));
                        self.ip = ip;
                        continue;
                    }
                    Some(Value::RustBlock(lambda)) => {
                        let ip = self.ip;
                        match lambda(self) {
                            Ok(()) => {
                                // skip incrementing ip if the block
                                // manipulated it
                                if ip != self.ip {
                                    continue;
                                }
                            }
                            Err(err) => {
                                return Err(err);
                            }
                        }
                    }
                    Some(Value::Nil(_)) => {
                        return Err(VMError {
                            err: "Message not understood.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                    _ => {
                        return Err(VMError {
                            err: "Attempt to call non-lambda value.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Lookup => match self.stack.pop() {
                    Some(Value::String(_, s)) => match self.stack.last() {
                        Some(Value::Block(proto, _))
                        | Some(Value::Boolean(proto, _))
                        | Some(Value::Nil(proto))
                        | Some(Value::Number(proto, _))
                        | Some(Value::String(proto, _)) => {
                            let result;
                            match proto.borrow().lookup(s) {
                                Some(v) => {
                                    result = v;
                                }
                                None => {
                                    result = Value::Nil(self.nil.clone());
                                }
                            }
                            self.stack.push(result);
                        }
                        Some(Value::Object(obj)) => {
                            let result;
                            match obj.borrow().lookup(s) {
                                Some(v) => {
                                    result = v;
                                }
                                None => {
                                    result = Value::Nil(self.nil.clone());
                                }
                            }
                            self.stack.push(result);
                        }
                        Some(Value::RustBlock(_)) => {
                            return Err(VMError {
                                err: "Rustblock does not support lookup.".to_string(),
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
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                    _ => {
                        return Err(VMError {
                            err: "Lookup expects string.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Pop => match self.stack.pop() {
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                    _ => {}
                },
                Opcode::Ret => match self.callstack.pop() {
                    Some((_, ip)) => {
                        self.ip = ip;
                        continue; // skip incrementing ip
                    }
                    None => {
                        return Err(VMError {
                            err: "Call stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
                Opcode::Swap => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(a);
                            self.stack.push(b);
                        }
                        None => {
                            return Err(VMError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                    },
                    None => {
                        return Err(VMError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
            }
            if self.enable_tracing {
                for i in 0..self.stack.len() {
                    println!("  stack {}: {}", i, self.stack[self.stack.len() - 1 - i]);
                }
            }
            self.ip += 1;
        }
        Ok(())
    }

    pub fn new() -> VirtualMachine {
        let object = Rc::new(RefCell::new(Object::new()));

        let mut vm = VirtualMachine {
            callstack: Vec::new(),
            instructions: Vec::new(),
            ip: 0,
            stack: Vec::new(),
            object: object.clone(),
            boolean: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            nil: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            number: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            string: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            enable_tracing: false,
        };

        stdlib::create_standard_objects(&mut vm);
        vm
    }
}

#[cfg(test)]
mod tests {
    use crate::vm;
    use std::rc::Rc;

    macro_rules! run {
        ($input:expr, $ip:expr, $type:ident, $value:expr) => {{
            let mut vm = vm::VirtualMachine::new();
            vm.ip = $ip;
            match vm.assemble($input) {
                Ok(()) => match vm.run() {
                    Ok(()) => {
                        assert_eq!(vm.ip, vm.instructions.len());
                        assert_eq!(vm.stack.len(), 1);
                        match vm.stack.pop() {
                            Some(vm::Value::$type(_, v)) => {
                                assert_eq!(v, $value);
                            }
                            _ => {
                                assert_eq!("Incorrect type.", "");
                            }
                        }
                    }
                    Err(e) => {
                        assert_eq!("Run failed.", e.err);
                    }
                },
                _ => {
                    assert_eq!("Assemble failed.", "");
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
            0,
            Number,
            3.0
        );

        run!(
            "
            const 1.0
            const 2.0
            div
        ",
            0,
            Number,
            0.5
        );

        run!(
            "
            const 1.0
            const 2.0
            mul
        ",
            0,
            Number,
            2.0
        );

        run!(
            "
            const 1.0
            const 2.0
            sub
        ",
            0,
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
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 2.0
            le
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            gt
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const true
            const false
            eq
        ",
            0,
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
            0,
            Boolean,
            false
        );

        run!(
            "
            const 'hello
            const 'hello
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const '
            const '
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 'hello
            const 'world
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 'hello
            const 2
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const nil
            const nil
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const true
            const nil
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const true
            const nil
            ne
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
        ",
            0,
            Boolean,
            false
        );

        runfails!(
            "
            const 1.0
            lookup
        ",
            "Lookup expects string."
        );

        runfails!(
            "
            lookup
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const 'something
            lookup
        ",
            "Stack underflow."
        );

        let mut vm = vm::VirtualMachine::new();
        match vm.assemble(
            "
            const true
            const 'prototype
            lookup
            call
        ",
        ) {
            Ok(()) => match vm.run() {
                Ok(()) => {
                    assert_eq!(vm.stack.len(), 1);
                    assert_eq!(vm.ip, 4);
                    match vm.stack.pop() {
                        Some(vm::Value::Object(obj)) => {
                            assert!(Rc::ptr_eq(&obj, &vm.boolean));
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

        let mut vm = vm::VirtualMachine::new();
        match vm.assemble(
            "
            const true
            const 'prototype
            lookup
            call
            const 'prototype
            lookup
            call
        ",
        ) {
            Ok(()) => match vm.run() {
                Ok(()) => {
                    assert_eq!(vm.stack.len(), 1);
                    assert_eq!(vm.ip, 7);
                    match vm.stack.pop() {
                        Some(vm::Value::Object(obj)) => {
                            assert!(Rc::ptr_eq(&obj, &vm.object));
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

        runfails!(
            "
            const 2
            const 'unknown
            lookup
            call
        ",
            "Message not understood."
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
            const 'not
            lookup
            call
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 'hello world
        ",
            0,
            String,
            "hello world"
        );

        run!(
            "
            const 1
            const 1
            pop
        ",
            0,
            Number,
            1.0
        );

        run!(
            "
            const 42
            ret
            block 0
            call
        ",
            2,
            Number,
            42.0
        );

        run!(
            "
            const 1
            const 1
            add
            ret
            block 0
            call
        ",
            4,
            Number,
            2.0
        );

        run!(
            "
            const 1
            add
            ret
            const 1
            block 0
            call
        ",
            3,
            Number,
            2.0
        );

        run!(
            "
            arg 0
            const 1
            add
            swap
            pop
            ret
            const 1
            block 0
            call
        ",
            6,
            Number,
            2.0
        );
    }
}
