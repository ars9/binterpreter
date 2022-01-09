use std::{collections::HashMap, panic};

use std::convert::TryInto;

type Value = i64;
type Var = u32;
type Label = u32;

#[derive(Debug)]
enum OpCode {
    LoadVal,
    WriteVar,
    ReadVar,
    Add,
    Multiply,
    ReturnValue,
    Label,
    Compare,
    Jump,
    JumpEq,
    Unknown,
}

impl From<u8> for OpCode {
    fn from(orig: u8) -> Self {
        match orig {
            0x0 => OpCode::LoadVal,
            0x1 => OpCode::WriteVar,
            0x2 => OpCode::ReadVar,
            0x3 => OpCode::Add,
            0x4 => OpCode::Multiply,
            0x5 => OpCode::ReturnValue,
            0x6 => OpCode::Label,
            0x7 => OpCode::Compare,
            0x8 => OpCode::Jump,
            0x9 => OpCode::JumpEq,
            _ => OpCode::Unknown,
        }
    }
}

#[derive(Debug)]
enum ByteCode {
    LoadVal(Value), // 0
    WriteVar(Var),  // 1
    ReadVar(Var),   // 2
    Add,            // 3
    Multiply,       // 4
    ReturnValue,    // 5
    Label(Label),   // 6
    Compare,        // 7
    Jump(Label),    // 8
    JumpEq(Label),  // 9
}

fn main() {
    let mut args = std::env::args().into_iter();
    let executable = args.next().unwrap();

    if let Some(path) = args.next() {
        match std::fs::read(path) {
            Ok(bytes) => {
                let result = exec(bytes);
                println!("Result: {:?}", result);
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::PermissionDenied {
                    eprintln!("please run again with appropriate permissions.");
                    return;
                }
                panic!("{}", e);
            }
        }
    } else {
        println!("Usage: {:} PATH_TO_BINARY", executable);
    }
}

type Cursor = usize;

#[derive(Debug)]
enum ParserError {
    ValueError(OpCode, Cursor),
    VariableError(OpCode, Cursor),
    LabelError(OpCode, Cursor),
    UnknownOpcode(Cursor),
    EmptyProgram,
}

type Line = usize;

#[derive(Debug)]
enum ProgramError {
    StackError(Line),
    UninitializedVariable(Line),
    LabelError(Line),
}

fn parse(bytes: &Vec<u8>) -> Result<Vec<ByteCode>, ParserError> {
    if bytes.len() == 0 {
        return Err(ParserError::EmptyProgram);
    }

    let mut cursor: usize = 0;
    let mut program: Vec<ByteCode> = vec![];

    let val_size = std::mem::size_of::<Value>();
    let var_size = std::mem::size_of::<Var>();
    let label_size = std::mem::size_of::<Label>();

    loop {
        let opcode: OpCode = bytes[cursor].into();

        cursor += 1;

        let bytecode = match opcode {
            OpCode::LoadVal => {
                let val = match bytes[cursor..cursor + val_size].try_into() {
                    Ok(val) => i64::from_le_bytes(val),
                    Err(_) => return Err(ParserError::ValueError(opcode, cursor)),
                };
                cursor += val_size;
                ByteCode::LoadVal(val)
            }
            OpCode::WriteVar => {
                let var = match bytes[cursor..cursor + var_size].try_into() {
                    Ok(var) => u32::from_le_bytes(var),
                    Err(_) => return Err(ParserError::VariableError(opcode, cursor)),
                };
                cursor += var_size;
                ByteCode::WriteVar(var)
            }
            OpCode::ReadVar => {
                let var = match bytes[cursor..cursor + var_size].try_into() {
                    Ok(var) => u32::from_le_bytes(var),
                    Err(_) => return Err(ParserError::VariableError(opcode, cursor)),
                };
                cursor += var_size;
                ByteCode::ReadVar(var)
            }
            OpCode::Add => ByteCode::Add,
            OpCode::Multiply => ByteCode::Multiply,
            OpCode::ReturnValue => ByteCode::ReturnValue,
            OpCode::Label => {
                let label = match bytes[cursor..cursor + label_size].try_into() {
                    Ok(label) => u32::from_le_bytes(label),
                    Err(_) => return Err(ParserError::LabelError(opcode, cursor)),
                };
                cursor += label_size;
                ByteCode::Label(label)
            }
            OpCode::Compare => ByteCode::Compare,
            OpCode::Jump => {
                let label = match bytes[cursor..cursor + label_size].try_into() {
                    Ok(label) => u32::from_le_bytes(label),
                    Err(_) => return Err(ParserError::LabelError(opcode, cursor)),
                };
                cursor += label_size;
                ByteCode::Jump(label)
            }
            OpCode::JumpEq => {
                let label = match bytes[cursor..cursor + label_size].try_into() {
                    Ok(label) => u32::from_le_bytes(label),
                    Err(_) => return Err(ParserError::LabelError(opcode, cursor)),
                };
                cursor += label_size;
                ByteCode::JumpEq(label)
            }
            OpCode::Unknown => {
                println!("Unknown opcode: {:?}", bytes[cursor]);
                return Err(ParserError::UnknownOpcode(cursor));
            }
        };

        program.push(bytecode);

        if cursor == bytes.len() {
            break;
        }
    }

    Ok(program)
}

fn exec(bytes: Vec<u8>) -> Option<Value> {
    let program = match parse(bytes.as_ref()) {
        Ok(program) => program,
        Err(error) => match error {
            ParserError::EmptyProgram => {
                panic!("Empty program");
            }
            ParserError::UnknownOpcode(cursor) => {
                panic!("Unknown opcode at position {:}", cursor);
            }
            ParserError::ValueError(opcode, cursor) => {
                panic!(
                    "Couldn't read value for opcode {:?} at position {:}",
                    opcode, cursor
                );
            }
            ParserError::VariableError(opcode, cursor) => {
                panic!(
                    "Couldn't read variable for opcode {:?} at position {:}",
                    opcode, cursor
                );
            }
            ParserError::LabelError(opcode, cursor) => {
                panic!(
                    "Couldn't read lable for opcode {:?} at position {:}",
                    opcode, cursor
                )
            }
        },
    };

    let mut stack: Vec<Value> = vec![];
    let mut result: Option<Value> = None;
    let mut variables: HashMap<Var, Value> = HashMap::new();
    let mut labels: HashMap<Label, usize> = HashMap::new();

    // Build label pointers before execution
    let mut cursor: usize = 0;
    for bytecode in &program {
        cursor += 1;
        if let ByteCode::Label(label) = bytecode {
            labels.insert(*label, cursor);
        }
    }

    let mut cursor: usize = 0;
    loop {
        let bytecode = &program[cursor];

        cursor += 1;

        let result = match bytecode {
            ByteCode::LoadVal(val) => {
                stack.push(*val);
                Ok(())
            }
            ByteCode::Add => {
                let v1 = stack.pop();
                let v2 = stack.pop();
                if v1.is_none() || v2.is_none() {
                    Err(ProgramError::StackError(cursor))
                } else {
                    stack.push(v1.unwrap() + v2.unwrap());
                    Ok(())
                }
            }
            ByteCode::Multiply => {
                let v1 = stack.pop();
                let v2 = stack.pop();

                if v1.is_none() || v2.is_none() {
                    Err(ProgramError::StackError(cursor))
                } else {
                    stack.push(v1.unwrap() * v2.unwrap());
                    Ok(())
                }
            }
            ByteCode::ReturnValue => {
                result = stack.pop();
                Ok(())
            }
            ByteCode::WriteVar(var) => {
                let val = stack.pop();

                if val.is_none() {
                    Err(ProgramError::StackError(cursor))
                } else {
                    variables.insert(*var, val.unwrap());
                    Ok(())
                }
            }
            ByteCode::ReadVar(var) => {
                let val = variables.get(&var);
                if val.is_none() {
                    Err(ProgramError::UninitializedVariable(cursor))
                } else {
                    let val = *val.unwrap();
                    stack.push(val);
                    Ok(())
                }
            }
            ByteCode::Label(_) => Ok(()),
            ByteCode::Compare => {
                let mut result: Value = 0;

                let v1 = stack.pop();
                let v2 = stack.pop();

                if v1.is_none() || v2.is_none() {
                    Err(ProgramError::UninitializedVariable(cursor))
                } else {
                    if v1 > v2 {
                        result = 1;
                    } else if v1 < v2 {
                        result = -1;
                    }
                    stack.push(result);
                    Ok(())
                }
            }
            ByteCode::Jump(label) => {
                let pointer = labels.get(label);

                if pointer.is_none() {
                    Err(ProgramError::LabelError(cursor))
                } else {
                    cursor = *pointer.unwrap();
                    Ok(())
                }
            }
            ByteCode::JumpEq(label) => {
                let pointer = labels.get(label);

                let val = stack.pop();

                if val.is_none() {
                    Err(ProgramError::StackError(cursor))
                } else if pointer.is_none() {
                    Err(ProgramError::LabelError(cursor))
                } else {
                    if val.unwrap() == 0 {
                        cursor = *pointer.unwrap();
                    }
                    Ok(())
                }
            }
        };

        if result.is_err() {
            match result.err().unwrap() {
                ProgramError::StackError(line) => {
                    panic!("{:?} (line #{:}): Can't pop stack", bytecode, line);
                }
                ProgramError::UninitializedVariable(line) => {
                    panic!("{:?} (line #{:}): Uninitialized variable", bytecode, line);
                }
                ProgramError::LabelError(line) => {
                    panic!("{:?} (line #{:}): Uninitialized label", bytecode, line);
                }
            }
        }

        if cursor >= program.len() {
            break;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn empty_program() {
        let bytes: Vec<u8> = [].to_vec();
        exec(bytes);
    }

    #[test]
    #[should_panic]
    fn load_val_error() {
        let bytes: Vec<u8> = [0, 1].to_vec();
        exec(bytes);
    }

    #[test]
    #[should_panic]
    fn stack_error() {
        let bytes: Vec<u8> = [1, 0, 0, 0, 0].to_vec();
        exec(bytes);
    }

    #[test]
    fn basic_add() {
        let bytes: Vec<u8> = [
            0, 1, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 1
            0, 2, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 2
            3, // ADD
            5, // RETURN_VALUE
        ]
        .to_vec();

        assert_eq!(exec(bytes), Some(3));
    }

    #[test]
    fn basic_mul() {
        let bytes: Vec<u8> = [
            0, 10, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 1
            0, 10, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 2
            4, // MULTIPLY
            5, // RETURN_VALUE
        ]
        .to_vec();

        assert_eq!(exec(bytes), Some(100));
    }

    #[test]
    fn case_1() {
        let bytes: Vec<u8> = [
            0, 1, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 1
            1, 0, 0, 0, 0, // WRITE_VAR 'x'
            0, 2, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 2
            1, 1, 0, 0, 0, // WRITE_VAR 'y'
            2, 0, 0, 0, 0, // READ_VAR 'x'
            0, 1, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 1
            3, // ADD
            2, 1, 0, 0, 0, // READ_VAR 'y'
            4, // MULTIPLY
            5, // RETURN_VALUE
        ]
        .to_vec();

        assert_eq!(exec(bytes), Some(4));
    }

    #[test]
    fn loop_test() {
        let bytes: Vec<u8> = [
            0, 10, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 10
            1, 0, 0, 0, 0, // WRITE_VAR 'x'
            0, 0, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 0
            1, 1, 0, 0, 0, // WRITE_VAR 'y'
            6, 0, 0, 0, 0, // LABEL 'loop'
            2, 1, 0, 0, 0, // READ_VAR 'y'
            0, 1, 0, 0, 0, 0, 0, 0, 0, // LOAD_VAL 1
            3, // ADD
            1, 1, 0, 0, 0, // WRITE_VAR 'y'
            2, 0, 0, 0, 0, // READ_VAR 'x'
            2, 1, 0, 0, 0, // READ_VAR 'y'
            7, // CMP
            9, 1, 0, 0, 0, // JUMP_EQ 'exit'
            8, 0, 0, 0, 0, // JUMP 'loop'
            6, 1, 0, 0, 0, // LABEL 'exit'
            2, 1, 0, 0, 0, // READ_VAR 'y'
            5, // RETURN_VALUE
        ]
        .to_vec();

        assert_eq!(exec(bytes), Some(10));
    }
}
