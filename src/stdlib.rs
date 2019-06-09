use crate::vm;

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

fn object_prototype(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(value) => match value {
            vm::Value::Block(proto, _)
            | vm::Value::Boolean(proto, _)
            | vm::Value::Nil(proto)
            | vm::Value::Number(proto, _)
            | vm::Value::String(proto, _) => {
                vm.stack.push(vm::Value::Object(proto.clone()));
                Ok(())
            }
            vm::Value::Object(obj) => {
                let result;
                match &obj.borrow().prototype {
                    Some(proto) => result = vm::Value::Object(proto.clone()),
                    None => result = vm::Value::Nil(vm.object.clone()),
                }
                vm.stack.push(result);
                Ok(())
            }
            vm::Value::RustBlock(_) => unreachable!(),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn object_set_with(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Object(obj)) => match vm.stack.pop() {
            Some(value) => match vm.stack.pop() {
                Some(vm::Value::String(_, s)) => {
                    obj.borrow_mut().members.insert(s.to_string(), value);
                    vm.stack.push(vm::Value::Object(obj.clone()));
                    Ok(())
                }
                None => Err(vm::VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                }),
                _ => Err(vm::VMError {
                    err: "set: expects string.".to_string(),
                    line: usize::max_value(),
                }),
            },
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
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
    vm.object.borrow_mut().members.insert(
        "prototype".to_string(),
        vm::Value::RustBlock(object_prototype),
    );
    vm.object.borrow_mut().members.insert(
        "set:with:".to_string(),
        vm::Value::RustBlock(object_set_with),
    );

    vm.block.borrow_mut().members.insert(
        "disassemble".to_string(),
        vm::Value::RustBlock(block_disassemble),
    );
    vm.block
        .borrow_mut()
        .members
        .insert("value".to_string(), vm::Value::RustBlock(block_value));

    vm.boolean
        .borrow_mut()
        .members
        .insert("and:".to_string(), vm::Value::RustBlock(boolean_and));
    vm.boolean.borrow_mut().members.insert(
        "ifFalse:".to_string(),
        vm::Value::RustBlock(boolean_iffalse),
    );
    vm.boolean.borrow_mut().members.insert(
        "ifTrue:ifFalse:".to_string(),
        vm::Value::RustBlock(boolean_iftrue_iffalse),
    );
    vm.boolean
        .borrow_mut()
        .members
        .insert("ifTrue:".to_string(), vm::Value::RustBlock(boolean_iftrue));
    vm.boolean
        .borrow_mut()
        .members
        .insert("not".to_string(), vm::Value::RustBlock(boolean_not));
    vm.boolean
        .borrow_mut()
        .members
        .insert("or:".to_string(), vm::Value::RustBlock(boolean_or));
}
