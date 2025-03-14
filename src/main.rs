use std::{env, process::{exit, Command}};
mod lexer;
mod parser;
mod assembler;
mod code_emission;

#[derive(Debug)]
enum ArgOption {
    None,
    Lex,
    Parser,
    Codegen,
    Source
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
    let assembly_file = compile(filename.clone(), option);
    Command::new("rm").arg(filename.clone()+".i").output().unwrap();
    
    //linker
    if !assembly_file.is_empty() {
        Command::new("gcc").args([assembly_file.clone(),"-o".to_string(),filename.clone()]).output().unwrap();
        println!("Output file at {}",filename.clone());
        Command::new("rm").arg(assembly_file.clone()).output().unwrap();
    }

    exit(0);
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
            "-S" => ArgOption::Source,
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

fn compile(filename: String, option: ArgOption) -> String {
    match option {
        ArgOption::Lex => {
            let tokens = lexer::lexer(filename.clone());
            println!("{:?}",tokens);
            String::new()
        },
        ArgOption::Parser => {
            let tokens = lexer::lexer(filename.clone());
            let parsed_program = parser::parser(tokens);
            println!("{}",parser::pretty_printer(&parsed_program, 0));
            String::new()
        },
        ArgOption::Codegen => {
            let tokens = lexer::lexer(filename.clone());
            let parsed_program = parser::parser(tokens);
            let asm_program = assembler::assembler(parsed_program);
            println!("{}",assembler::pretty_printer(&asm_program, 0));
            String::new()
        },
        ArgOption::None => {
            let tokens = lexer::lexer(filename.clone());
            let parsed_program = parser::parser(tokens);
            let asm_program = assembler::assembler(parsed_program);
            code_emission::code_emission(asm_program, filename)
        },
        ArgOption::Source => {
            let tokens = lexer::lexer(filename.clone());
            let parsed_program = parser::parser(tokens);
            let asm_program = assembler::assembler(parsed_program);
            code_emission::code_emission(asm_program, filename);
            String::new()
        }
    }
}