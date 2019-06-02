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

pub enum Opcode {
    Add,
    Div,
    Mul,
    ConstF(usize),
    Sub,
}

impl fmt::Display for Opcode {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::Add => write!(f, "add"),
            Opcode::Div => write!(f, "div"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::ConstF(idx) => write!(f, "const {}", idx),
            Opcode::Sub => write!(f, "sub"),
        }
    }
}

#[derive(Default)]
pub struct VirtualMachine {
    pub fconsts: Vec<f64>,
    pub stack: Vec<f64>,
    pub instructions: Vec<Opcode>,
    pub ip: usize,
}

impl VirtualMachine {
    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.ip = 0;
        while self.ip < self.instructions.len() {
            match &self.instructions[self.ip] {
                Opcode::Add => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(a + b);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },

                Opcode::Div => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(b / a);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },

                Opcode::ConstF(idx) => match self.fconsts.get(*idx) {
                    Some(n) => {
                        self.stack.push(*n);
                    }
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },

                Opcode::Mul => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(a * b);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },

                Opcode::Sub => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(b - a);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: usize::max_value(),
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: usize::max_value(),
                        });
                    }
                },
            }
            self.ip += 1;
        }
        Ok(())
    }

    pub fn new() -> VirtualMachine {
        VirtualMachine {
            fconsts: Vec::new(),
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
                assert_eq!(vm.fconsts.len(), 0);
                assert_eq!(vm.stack.len(), 0);
                assert_eq!(vm.ip, 0);
            }
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        vm.fconsts.push(1.0);
        vm.fconsts.push(2.0);
        vm.instructions.push(vm::Opcode::ConstF(0));
        vm.instructions.push(vm::Opcode::ConstF(1));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(n) => {
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
        vm.fconsts.push(1.0);
        vm.fconsts.push(2.0);
        vm.instructions.push(vm::Opcode::ConstF(0));
        vm.instructions.push(vm::Opcode::ConstF(1));
        vm.instructions.push(vm::Opcode::Div);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(n) => {
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
        vm.fconsts.push(3.0);
        vm.fconsts.push(6.0);
        vm.instructions.push(vm::Opcode::ConstF(0));
        vm.instructions.push(vm::Opcode::ConstF(1));
        vm.instructions.push(vm::Opcode::Mul);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(n) => {
                        assert_eq!(n, 18.0);
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
        vm.fconsts.push(3.0);
        vm.fconsts.push(6.0);
        vm.instructions.push(vm::Opcode::ConstF(0));
        vm.instructions.push(vm::Opcode::ConstF(1));
        vm.instructions.push(vm::Opcode::Sub);
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 1);
                assert_eq!(vm.ip, 3);

                match vm.stack.pop() {
                    Some(n) => {
                        assert_eq!(n, -3.0);
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
        vm.fconsts.push(3.0);
        vm.instructions.push(vm::Opcode::ConstF(0));
        vm.instructions.push(vm::Opcode::Add);
        match vm.run() {
            Ok(()) => {
                assert!(false);
            }
            Err(e) => {
                assert_eq!(e.err, "Stack underflow.");
            }
        }
    }
}
