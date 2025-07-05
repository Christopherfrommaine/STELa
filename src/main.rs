mod parser;
mod interpreter;
mod interpreter2;
mod format_tree;

use env_logger;

const PRELUDE: &str = include_str!("prelude.st");

fn main() {
    env_logger::init();

    let path = std::env::args().into_iter().skip(1).next().unwrap_or("examples/test3.st".to_string());
    let file_contents = std::fs::read_to_string(path);

    match file_contents {
        Ok(s) => {
            let l = parser::Lexer::new(&(s));
            let mut p = parser::Parser::new(l);
            let res = p.parse();
            let o: String;
            match res {
                Ok(prog) => {o = format!("Parsed Program:\n{:?}", prog);},
                Err(reason) => {o = format!("{}", reason);}
            }
            // println!("{}", format_tree::format_tree(&o));
            println!("{}", o);
        },
        Err(e) => {eprintln!("No file found:\n{:?}", e);},
    }
}
