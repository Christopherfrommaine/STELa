mod parser;
mod interpreter;
mod format_tree;

use env_logger;

const PRELUDE: &str = include_str!("prelude.stela");

fn main() {
    env_logger::init();

    let path = std::env::args().into_iter().skip(1).next().unwrap_or("examples/test2.stela".to_string());
    let file_contents = std::fs::read_to_string(path);

    match file_contents {
        Ok(s) => {
            let l = parser::Lexer::new(&(PRELUDE.to_string() + &s));
            let mut p = parser::Parser::new(l);
            let res = p.parse();
            
            match res {
                Ok(prog) => {
                    println!("Parsed Program: \n{:?}\nEnd Parsed Program.", prog);

                    let mut interp = interpreter::Interpreter::new(prog.statements);

                    match interp.run() {
                        Err(e) => {println!("--- error:\n{}", e);},
                        Ok(s) => {println!("--- output:\n{}", s);}
                    }
                
                },
                Err(reason) => {println!("{}", reason);}
            }
        },
        Err(e) => {eprintln!("No file found:\n{:?}", e);},
    }
}
