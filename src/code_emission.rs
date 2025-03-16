use std::fs::File;
use std::io::Write;

use crate::assembler::{AsmNode, BinaryOp, Instruction, Operand, UnaryOp};

pub fn code_emission(program: AsmNode, filename: String) -> String {
    let mut out_file = File::create(filename.clone() + ".s").unwrap();
    write!(out_file, "{}", code_gen(program)).unwrap();
    filename + ".s"
}

fn code_gen(program: AsmNode) -> String {
    match program {
        AsmNode::Program(func) => code_gen(*func),
        AsmNode::Function(name, instructions) => {
            let mut output = format!(
                "\t.globl _{}\n_{}:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp",
                name, name
            );
            for instruction in instructions {
                output += &format!("\n{}", code_gen_instr(instruction));
            }
            output
        }
    }
}

fn code_gen_instr(instruction: Instruction) -> String {
    match instruction {
        Instruction::Mov(src, dst) => format!("\tmovl\t{}, {}", code_gen_op(src), code_gen_op(dst)),
        Instruction::Unary(unop, operand) => {
            format!("\t{}\t{}", code_gen_unop(unop), code_gen_op(operand))
        }
        Instruction::AllocateStack(bytes) => format!("\tsubq\t${}, %rsp", bytes),
        Instruction::Ret => String::from("\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"),
        Instruction::Binary(binop, src, dst) => format!("\t{}\t{}, {}",code_gen_binop(binop),code_gen_op(src),code_gen_op(dst)),
        Instruction::Idiv(operand) => format!("\tidivl\t{}",code_gen_op(operand)),
        Instruction::Cdq => String::from("\tcdq"),
    }
}

fn code_gen_op(operand: Operand) -> String {
    match operand {
        Operand::Imm(val) => format!("${}", val),
        Operand::Reg(register) => match register {
            crate::assembler::Register::AX => String::from("%eax"),
            crate::assembler::Register::R10 => String::from("%r10d"),
            crate::assembler::Register::DX => String::from("%edx"),
            crate::assembler::Register::R11 => String::from("%r11d"),
        },
        Operand::Stack(bytes) => format!("{}(%rbp)", bytes),
        Operand::Pseudo(_) => unreachable!("Pseudoregister not removed"),
    }
}

fn code_gen_unop(unop: UnaryOp) -> String {
    match unop {
        UnaryOp::Neg => String::from("negl"),
        UnaryOp::Not => String::from("notl"),
    }
}

fn code_gen_binop(binop: BinaryOp) -> String {
    match binop {
        BinaryOp::Add => String::from("addl"),
        BinaryOp::Sub => String::from("subl"),
        BinaryOp::Mult => String::from("imull"),
    }
}