mod parser;
mod interpreter;
mod format_tree;

use env_logger;

const PRELUDE: &str = include_str!("prelude.stela");

fn main() {
    env_logger::init();

    let path = std::env::args().into_iter().skip(1).next().unwrap_or("examples/test1.stela".to_string());
    let file_contents = std::fs::read_to_string(path);

    match file_contents {
        Ok(s) => {
            let l = parser::Lexer::new(&(s));
            let mut p = parser::Parser::new(l);
            let res = p.parse();
            let o: String;
            match res {
                Ok(prog) => {
                    println!("Parsed Program: \n{:?}", prog);
                    
                    let mut interp = interpreter::Interpreter::new(prog.statements);

                    let mut o = Ok(());
                    while Ok(()) == o {
                        o = interp.advance();
                    }

                    println!("o: {o:?}");
                
                },
                Err(reason) => {println!("{}", reason);}
            }
        },
        Err(e) => {eprintln!("No file found:\n{:?}", e);},
    }
}
