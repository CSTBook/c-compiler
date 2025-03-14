use std::{env, process::Command};
mod lexer;
mod parser;
mod assembler;
mod code_emission;

#[derive(Debug)]
enum ArgOption {
    None,
    Lex,
    Parser,
    Codegen
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("Not enough arguments passed in");
    }

    let (filename, option) = arg_parser(args);

    //preprocess
    Command::new("gcc").args(["-E","-P",&(filename.clone()+".c"),"-o",&(filename.clone()+".i")]).output().unwrap();

    //compile
    // Command::new("rm").arg(&(filename.clone()+".i")).output().unwrap();
    compile(filename.clone(), option);

    //linker
    let _assembly_file="";
    // Command::new("gcc").args([assembly_file,"-o",&(filename.clone()+".o")]).output().unwrap();
    
}

//input is of the form cargo run -- --option filename

fn arg_parser(args: Vec<String>) -> (String, ArgOption) {
    let mut filename = args[1].split(".").next().unwrap().to_string();

    let option;
    if args.len()>=3 {
        option = match args[1].as_str() {
            "--lex" => ArgOption::Lex,
            "--parse" => ArgOption::Parser,
            "--codegen" => ArgOption::Codegen,
            _ => ArgOption::None
        };
        filename = args[2].split(".").next().unwrap().to_string();
    } else {
        option = ArgOption::None;
    }
    println!("Filename: {}.c",filename);

    println!("Option passed: {:?}", option);

    (filename, option)
}

fn compile(filename: String, _option: ArgOption) {
    let tokens = lexer::lexer(filename.clone());
    let parsed_program = parser::parser(tokens);
    let asm_program = assembler::assembler(parsed_program);

}