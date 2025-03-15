use std::collections::HashMap;

use crate::{
    parser::{self},
    tacky::{TackyInstruction, TackyNode, TackyValue},
};

pub enum AsmNode {
    Program(Box<AsmNode>),              //func
    Function(String, Vec<Instruction>), //name, instructions
}

#[derive(Clone,PartialEq)]
pub enum Unary {
    Neg,
    Not,
}
#[derive(Clone,PartialEq)]
pub enum Register {
    AX,
    R10,
}

#[derive(Clone,PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand), //src, dst
    Unary(Unary, Operand), //unop, operand
    AllocateStack(i32),
    Ret,
}
#[derive(Clone,PartialEq)]
pub enum Operand {
    Imm(i32),       //val
    Reg(Register),  //reg
    Pseudo(String), //identifier
    Stack(i32),
}

pub fn assembler(program_ast: TackyNode) -> AsmNode {
    let mut program = parse_program(program_ast);
    let offset = replace_pseudoregister(&mut program);
    fix_instructions(offset, &mut program);

    program
}

fn parse_program(program_ast: TackyNode) -> AsmNode {
    match program_ast {
        TackyNode::Program(func) => AsmNode::Program(Box::new(parse_function(*func))),
        _ => panic!("Tacky Program Assembler Error"),
    }
}

fn parse_function(function_ast: TackyNode) -> AsmNode {
    match function_ast {
        TackyNode::Function(name, instructions) => {
            AsmNode::Function(name, parse_instruction(instructions))
        }
        _ => panic!("Tacky Function Assembler Error"),
    }
}

fn parse_instruction(tacky_instructions: Vec<TackyInstruction>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();

    for instruction in tacky_instructions {
        match instruction {
            TackyInstruction::Return(val) => {
                instructions.push(Instruction::Mov(
                    parse_operand(&val),
                    Operand::Reg(Register::AX),
                ));
                instructions.push(Instruction::Ret);
            }
            TackyInstruction::Unary(unop, src, dest) => {
                instructions.push(Instruction::Mov(parse_operand(&src), parse_operand(&dest)));
                instructions.push(Instruction::Unary(parse_unop(unop), parse_operand(&dest)));
            }
            // _ => panic!("Tacky Instruction Assembler Error"),
        }
    }

    instructions
}

fn parse_operand(op_ast: &TackyValue) -> Operand {
    match op_ast {
        TackyValue::Constant(val) => Operand::Imm(*val),
        TackyValue::Var(name) => Operand::Pseudo(name.clone()),
        // _ => panic!("Tacky Operand Assembler Error"),
    }
}

fn parse_unop(unop: parser::Unary) -> Unary {
    match unop {
        parser::Unary::Complement => Unary::Not,
        parser::Unary::Negate => Unary::Neg,
    }
}

fn replace_pseudoregister(program: &mut AsmNode) -> i32 {
    let mut temp_vars: HashMap<String, i32> = HashMap::new();

    if let AsmNode::Program(func) = program {
        if let AsmNode::Function(_, ref mut instructions) = **func {
            for instruction in instructions.iter_mut() {
                *instruction = match instruction {
                    Instruction::Mov(src, dst) => {
                        let new_src = replace_operand(src, &mut temp_vars);
                        let new_dst = replace_operand(dst, &mut temp_vars);

                        Instruction::Mov(new_src, new_dst)
                    }
                    Instruction::Unary(unop, operand) => {
                        let new_op = replace_operand(operand, &mut temp_vars);

                        Instruction::Unary(unop.clone(), new_op)
                    }
                    _ => instruction.clone()
                };
            }
        }
    }

    -4*(temp_vars.len() as i32)    
}

fn replace_operand(operand: &Operand, temp_vars: &mut HashMap<String, i32>) -> Operand {
    if let Operand::Pseudo(name) = operand {
        if !temp_vars.contains_key(name) {
            temp_vars.insert(name.clone(), -4*(temp_vars.len() as i32 + 1));
        }
        Operand::Stack(*temp_vars.get(name).unwrap())
    } else {
        operand.clone()
    }
}

fn fix_instructions(offset: i32, program: &mut AsmNode) {
    if let AsmNode::Program(func) = program {
        if let AsmNode::Function(_, ref mut instructions) = **func {
            instructions.insert(0, Instruction::AllocateStack(offset));
            for index in 1..instructions.clone().len() {
                if let Instruction::Mov(src, dst) = instructions.clone().get(index).unwrap() {
                    if let (Operand::Stack(_),Operand::Stack(_)) = (src,dst) { //both stack ops
                        instructions.remove(index);
                        instructions.insert(index, Instruction::Mov(src.clone(), Operand::Reg(Register::R10)));
                        instructions.insert(index+1, Instruction::Mov(Operand::Reg(Register::R10),dst.clone()));
                    }
                }
            }
        }
    }
}

#[allow(dead_code)]
pub fn pretty_printer(node: &AsmNode, indent_level: usize) -> String {
    match node {
        AsmNode::Program(func) => {
            format!(
                "{}Program(\n{}{}\n{})",
                tabs(indent_level),
                tabs(indent_level),
                pretty_printer(func, indent_level + 1),
                tabs(indent_level)
            )
        }
        AsmNode::Function(name, instructions) => {
            let mut output = format!(
                "{}Function(\n{}name=\"{}\"\n{}body=(",
                tabs(indent_level),
                tabs(indent_level + 1),
                name,
                tabs(indent_level + 1)
            );
            for instruction in instructions {
                output += &format!("\n{}", pretty_printer_instr(instruction, indent_level + 2));
            }
            output += &format!("\n{})\n{})", tabs(indent_level + 1), tabs(indent_level));
            output
        }
    }
}

#[allow(dead_code)]
fn pretty_printer_instr(instruction: &Instruction, indent_level: usize) -> String {
    match instruction {
        Instruction::Mov(src, dest) => {
            format!(
                "{}Mov({},{})",
                tabs(indent_level),
                pretty_printer_opr(src),
                pretty_printer_opr(dest)
            )
        }
        Instruction::Unary(unop, operand) => format!(
            "{}Unary({},{})",
            tabs(indent_level),
            pretty_printer_unop(unop),
            pretty_printer_opr(operand)
        ),
        Instruction::AllocateStack(val) => format!("{}AllocateStack({})", tabs(indent_level), *val),
        Instruction::Ret => format!("{}Ret", tabs(indent_level)),
    }
}

fn pretty_printer_unop(unop: &Unary) -> String {
    match unop {
        Unary::Neg => String::from("Neg"),
        Unary::Not => String::from("Not"),
    }
}

fn pretty_printer_opr(operand: &Operand) -> String {
    match operand {
        Operand::Imm(val) => format!("Imm({})", *val),
        Operand::Reg(register) => format!("Reg({})", pretty_printer_reg(register)),
        Operand::Pseudo(name) => format!("Var({})", name),
        Operand::Stack(val) => format!("Stack({})", *val),
    }
}

fn pretty_printer_reg(register: &Register) -> String {
    match register {
        Register::AX => String::from("AX"),
        Register::R10 => String::from("R10"),
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}
