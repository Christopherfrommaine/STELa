mod parser;
mod interpreter;
mod format_tree;

use env_logger;

const PRELUDE: &str = include_str!("prelude.stela");

fn main() {
    env_logger::init();

    let args: Vec<String> = std::env::args().into_iter().skip(1).collect();
    let include_prelude = !args.contains(&"--no-prelude".to_string());
    let path = args
        .into_iter()
        .filter(|s| s.chars().next().unwrap_or(' ') != '-')  // Remove flags
        .next()
        .unwrap_or("examples/test2.stela".to_string());
    
    let mut inp =  std::fs::read_to_string(path).expect("File not found.");

    if include_prelude {
        inp = PRELUDE.to_string() + &inp
    }

    let program = parser::Parser::new(
            parser::Lexer::new(&inp)
        )
        .parse()
        .expect("Unable to parse.");

    
    println!("Parsed Program: \n{:?}\nEnd Parsed Program.", program);

    let mut interpreter = interpreter::Interpreter::new(program.statements);

    match interpreter.run() {
        Err(e) => {println!("--- error:\n{}", e);},
        Ok(s) => {println!("--- output:\n{}", s);}
    }
}
