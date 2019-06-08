use crate::vm;
use std::rc::Rc;

fn block_disassemble(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(_, ip)) => {
            let mut ip = ip;
            println!("lambda @{}", ip);
            loop {
                let instr = &vm.instructions[ip];
                println!("    {}", instr);
                match instr {
                    vm::Opcode::Ret => break,
                    _ => {}
                }
                ip += 1;
            }
            vm.stack.push(vm::Value::Nil(vm.object.clone()));
            Ok(())
        }
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn block_value(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(_, ip)) => {
            vm.callstack.push((vm.stack.len(), vm.ip + 1));
            vm.ip = ip;
            Ok(())
        }
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_and(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a && b));
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "and: expects boolean.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

// Interestingly, Smalltalk generates branch opcodes and inlines
// ifFalse:, ifTrue:ifFalse: and ifTrue: which means you can not
// define methods with these names for non-booleans.
fn boolean_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, ip)) => {
                if !a {
                    vm.callstack.push((vm.stack.len(), vm.ip + 1));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifFalse: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_iftrue(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, ip)) => {
                if a {
                    vm.callstack.push((vm.stack.len(), vm.ip + 1));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifTrue: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_iftrue_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, false_ip)) => match vm.stack.pop() {
                Some(vm::Value::Block(_, true_ip)) => {
                    if a {
                        vm.callstack.push((vm.stack.len(), vm.ip + 1));
                        vm.ip = true_ip;
                    } else {
                        vm.callstack.push((vm.stack.len(), vm.ip + 1));
                        vm.ip = false_ip;
                    }
                    Ok(())
                }
                None => Err(vm::VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                }),
                _ => Err(vm::VMError {
                    err: "ifTrue: expects block.".to_string(),
                    line: usize::max_value(),
                }),
            },
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifFalse: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_not(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(proto, b)) => {
            vm.stack.push(vm::Value::Boolean(proto.clone(), !b));
            Ok(())
        }
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_or(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a || b));
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "or: expects boolean.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

pub fn create_standard_objects(vm: &mut vm::VirtualMachine) {
    match Rc::get_mut(&mut vm.block) {
        Some(obj) => {
            obj.members.insert(
                "disassemble".to_string(),
                vm::Value::RustBlock(block_disassemble),
            );
            obj.members
                .insert("value".to_string(), vm::Value::RustBlock(block_value));
        }
        None => {}
    };

    match Rc::get_mut(&mut vm.boolean) {
        Some(obj) => {
            obj.members
                .insert("and:".to_string(), vm::Value::RustBlock(boolean_and));
            obj.members.insert(
                "ifFalse:".to_string(),
                vm::Value::RustBlock(boolean_iffalse),
            );
            obj.members.insert(
                "ifTrue:ifFalse:".to_string(),
                vm::Value::RustBlock(boolean_iftrue_iffalse),
            );
            obj.members
                .insert("ifTrue:".to_string(), vm::Value::RustBlock(boolean_iftrue));
            obj.members
                .insert("not".to_string(), vm::Value::RustBlock(boolean_not));
            obj.members
                .insert("or:".to_string(), vm::Value::RustBlock(boolean_or));
        }
        None => {}
    };
}
