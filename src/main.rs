use std::{
    env, fs,
    io::{self, BufRead, Write},
    process,
};

use trivial::{parser::syntax::SyntaxParser, scanner::Scanner, token::Token};

fn run(content: String) {
    let mut s = Scanner::new(&content);
    s.scan_tokens();

    let mut p: SyntaxParser<'_> =
        SyntaxParser::new(s.tokens.into_iter().flatten().collect::<Vec<Token>>());
    println!("{}", p.parse());
}

fn run_file(path: String) {
    let content = fs::read_to_string(path).expect("could not read from file");
    run(content);
}

fn run_prompt() {
    loop {
        print!("> ");
        io::stdout().lock().flush().expect("could not flush stdout");

        let mut input = String::new();
        let bytes = io::stdin()
            .lock()
            .read_line(&mut input)
            .expect("could not read from stdin");
        input = input.trim().to_string();

        if bytes == 0 {
            break;
        }

        run(input);
    }
}

fn main() {
    let args = env::args();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        run_file(args.last().unwrap());
    } else {
        run_prompt();
    }
}
