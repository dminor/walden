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
            obj.members
                .insert("not".to_string(), vm::Value::RustBlock(boolean_not));
            obj.members
                .insert("or:".to_string(), vm::Value::RustBlock(boolean_or));
        }
        None => {}
    };
}
