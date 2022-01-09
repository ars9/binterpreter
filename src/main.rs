use std::{
    collections::HashMap,
    convert::TryInto,
    env::{args, Args},
    fs::{read, File},
    io::{stdout, BufRead, BufReader, ErrorKind, Write},
    mem::size_of,
    panic,
};

use regex::Regex;

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

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        match self {
            OpCode::LoadVal => 0x0,
            OpCode::WriteVar => 0x1,
            OpCode::ReadVar => 0x2,
            OpCode::Add => 0x3,
            OpCode::Multiply => 0x4,
            OpCode::ReturnValue => 0x5,
            OpCode::Label => 0x6,
            OpCode::Compare => 0x7,
            OpCode::Jump => 0x8,
            OpCode::JumpEq => 0x9,
            OpCode::Unknown => panic!("OpCode doesn't exist"),
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

fn usage_full(basepath: &String) {
    println!("Usage: {:} <SUBCOMMAND> <OPTIONS>", basepath);
}

fn usage_exec(basepath: &String) {
    println!("Usage: {:} exec <PATH_TO_FILE>", basepath);
}

fn main() {
    let mut args = args().into_iter();
    let basepath = args.next().unwrap();

    if let Some(subcommand) = args.next() {
        match subcommand.as_ref() {
            "exec" => cmd_exec(&basepath, &mut args),
            "compile" => cmd_compile(&basepath, &mut args),
            _ => usage_full(&basepath),
        }
    } else {
        usage_full(&basepath);
    }
}

fn cmd_compile(basepath: &String, args: &mut Args) {
    if let Some(path) = args.next() {
        match File::open(path) {
            Ok(f) => {
                let buffer = BufReader::new(f);
                let result = compile(buffer).unwrap();
                let mut out = stdout();
                match out.write_all(&result) {
                    Ok(_) => out.flush().unwrap(),
                    Err(error) => {
                        panic!("Unable to write output: {}", error);
                    }
                }
            }
            Err(e) => {
                if e.kind() == ErrorKind::PermissionDenied {
                    eprintln!("please run again with appropriate permissions.");
                    return;
                }
                panic!("{}", e);
            }
        }
    } else {
        usage_exec(&basepath);
    }
}

fn cmd_exec(basepath: &String, args: &mut Args) {
    if let Some(path) = args.next() {
        match read(path) {
            Ok(bytes) => {
                let result = exec(bytes);
                println!("Result: {:?}", result);
            }
            Err(e) => {
                if e.kind() == ErrorKind::PermissionDenied {
                    eprintln!("please run again with appropriate permissions.");
                    return;
                }
                panic!("{}", e);
            }
        }
    } else {
        usage_exec(&basepath);
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

#[derive(Debug)]
enum CompilerError {}

fn compile(buffer: BufReader<File>) -> Result<Vec<u8>, CompilerError> {
    let mut output: Vec<u8> = vec![];

    let mut cursor = 0;
    let mut variables: HashMap<String, Var> = HashMap::new();
    let mut labels: HashMap<String, Label> = HashMap::new();

    let re_var_name = Regex::new(r"^[a-zA-Z$_]?[a-zA-Z0-9_]+$").unwrap();

    for line in buffer.lines() {
        cursor += 1;
        if line.is_err() {
            panic!("Can't read line from source");
        }
        let line = line.unwrap();
        let mut words = line.trim().split_whitespace();
        match words.next() {
            Some("LOAD_VAL") => {
                if let Some(val) = words.next() {
                    let val = val.parse::<Value>().unwrap();
                    output.push(OpCode::LoadVal.into());
                    output.extend_from_slice(&val.to_le_bytes());
                } else {
                    panic!("LOAD_VAL: missing value at line {}", cursor);
                }
            }
            Some("WRITE_VAR") => {
                if let Some(var) = words.next() {
                    if !re_var_name.is_match(var) {
                        panic!(
                            "WRITE_VAR: malformed variable name '{}' at line {}",
                            var, cursor
                        );
                    }
                    if !variables.contains_key(var) {
                        variables.insert(var.to_string(), variables.len().try_into().unwrap());
                    }
                    let var = variables.get(var).unwrap();
                    output.push(OpCode::WriteVar.into());
                    output.extend_from_slice(&var.to_le_bytes());
                } else {
                    panic!("WRITE_VAR: missing variable name at line {}", cursor);
                }
            }
            Some("READ_VAR") => {
                if let Some(var) = words.next() {
                    if !variables.contains_key(var) {
                        panic!(
                            "READ_VAR: Variable {} does not exist at line {}",
                            var, cursor
                        );
                    }
                    let var = variables.get(var).unwrap();
                    output.push(OpCode::ReadVar.into());
                    output.extend_from_slice(&var.to_le_bytes());
                } else {
                    panic!("READ_VAR: missing variable name at line {}", cursor);
                }
            }
            Some("ADD") => {
                output.push(OpCode::Add.into());
            }
            Some("MULTIPLY") => {
                output.push(OpCode::Multiply.into());
            }
            Some("RETURN_VALUE") => {
                output.push(OpCode::ReturnValue.into());
            }
            Some("LABEL") => {
                if let Some(label) = words.next() {
                    if !re_var_name.is_match(label) {
                        panic!("LABEL: malformed label name '{}' at line {}", label, cursor);
                    }
                    if !labels.contains_key(label) {
                        labels.insert(label.to_string(), labels.len().try_into().unwrap());
                    }
                    let label = labels.get(label).unwrap();
                    output.push(OpCode::Label.into());
                    output.extend_from_slice(&label.to_le_bytes());
                } else {
                    panic!("LABEL: missing label name at line {}", cursor);
                }
            }
            Some("CMP") => {
                output.push(OpCode::Compare.into());
            }
            Some("JUMP") => {
                if let Some(label) = words.next() {
                    if !re_var_name.is_match(label) {
                        panic!(
                            "JUMP: malformed label name '{}' at line {}",
                            label, cursor
                        );
                    }
                    if !labels.contains_key(label) {
                        labels.insert(label.to_string(), labels.len().try_into().unwrap());
                    }
                    let label = labels.get(label).unwrap();
                    output.push(OpCode::Jump.into());
                    output.extend_from_slice(&label.to_le_bytes());
                } else {
                    panic!("JUMP: missing label name at line {}", cursor);
                }
            }
            Some("JUMP_EQ") => {
                if let Some(label) = words.next() {
                    if !re_var_name.is_match(label) {
                        panic!(
                            "JUMP_EQ: malformed label name '{}' at line {}",
                            label, cursor
                        );
                    }
                    if !labels.contains_key(label) {
                        labels.insert(label.to_string(), labels.len().try_into().unwrap());
                    }
                    let label = labels.get(label).unwrap();
                    output.push(OpCode::JumpEq.into());
                    output.extend_from_slice(&label.to_le_bytes());
                } else {
                    panic!("JUMP_EQ: missing label name at line {}", cursor);
                }
            }
            Some("") => (),
            Some("#") => (),
            Some(operator) => panic!("{}: unknown operator at line {}", operator, cursor),
            None => (),
        }
    }

    Ok(output)
}

fn parse(bytes: &Vec<u8>) -> Result<Vec<ByteCode>, ParserError> {
    if bytes.len() == 0 {
        return Err(ParserError::EmptyProgram);
    }

    let mut cursor: usize = 0;
    let mut program: Vec<ByteCode> = vec![];

    let val_size = size_of::<Value>();
    let var_size = size_of::<Var>();
    let label_size = size_of::<Label>();

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
