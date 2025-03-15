use std::fs::File;
use std::io::Write;

use crate::assembler::{AsmNode, Instruction, Operand, Unary};

pub fn code_emission(program: AsmNode, filename: String) -> String {
    let mut out_file = File::create(filename.clone() + ".s").unwrap();
    write!(out_file, "{}", code_gen(program)).unwrap();
    filename + ".s"
}

fn code_gen(program: AsmNode) -> String {
    match program {
        AsmNode::Program(func) => code_gen(*func),
        AsmNode::Function(name, instructions) => {
            let mut output = format!("\t.globl _{}\n_{}:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp", name, name);
            for instruction in instructions {
                output += &format!("\n{}", code_gen_instr(instruction));
            }
            output
        } // AsmNode::Mov(src, dest) => format!("\tmovl {},{}", code_gen(*src), code_gen(*dest)),
          // AsmNode::Ret => String::from("\tret"),
          // AsmNode::Imm(val) => format!("\t${}", val),
          // AsmNode::Register => String::from("%eax"),
    }
}

fn code_gen_instr(instruction: Instruction) -> String {
    match instruction {
        Instruction::Mov(src, dst) => format!("\tmovl\t{}, {}",code_gen_op(src),code_gen_op(dst)),
        Instruction::Unary(unop, operand) => format!("\t{}\t{}",code_gen_unop(unop),code_gen_op(operand)),
        Instruction::AllocateStack(bytes) => format!("\tsubq\t${}, %rsp", bytes),
        Instruction::Ret => String::from("\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"),
    }
}

fn code_gen_op(operand: Operand) -> String {
    match operand {
        Operand::Imm(val) => format!("${}",val),
        Operand::Reg(register) => match register {
            crate::assembler::Register::AX => String::from("%eax"),
            crate::assembler::Register::R10 => String::from("%r10d"),
        },
        Operand::Stack(bytes) => format!("{}(%rbp)",bytes),
        _ => unreachable!("Pseudoregister not removed")
    }
}

fn code_gen_unop(unop: Unary) -> String {
    match unop {
        Unary::Neg => String::from("negl"),
        Unary::Not => String::from("notl"),
    }
}