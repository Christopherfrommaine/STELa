use log;
use core::fmt;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals and identifiers
    Identifier(String),
    
    // Operators and keywords
    Apply,
    Equals,
    In,
    None,
    ForAll,
    Print,
    Display,
    
    // Delimiters
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,

    Eof,
}

#[derive(Clone, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Identifier(String),
    SetLiteral(Vec<Expr>),
    Application(Box<Expr>, Box<Expr>),
    ForAll(Box<Expr>, String),
    Membership(Box<Expr>, Box<Expr>),
    Known(crate::interpreter::Set),  // Only used within interpreter
    None,                             // Only used within interpreter
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(name) => write!(f, "{}", name),
            Expr::SetLiteral(elems) => {write!(f, "{:?}", elems)}
            Expr::Application(func, arg) => write!(f, "({:?}@{:?})", func, arg),
            Expr::ForAll(expr, var) => {write!(f, "{{{:?} for all {}}}", expr, var)}
            Expr::Membership(elem, set) => write!(f, "{:?} in {:?}", elem, set),
            Expr::Known(s) => {write!(f, "{:?}", s)},
            Expr::None => {write!(f, "none")}
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l0), Self::Identifier(r0)) => l0 == r0,
            (Self::SetLiteral(l0), Self::SetLiteral(r0)) => {
                // let l0s: HashSet<Expr> = l0.iter().cloned().collect();
                // let r0s: HashSet<Expr> = r0.iter().cloned().collect();
                // l0s == r0s

                l0.iter().all(|i| r0.contains(i))
            },
            (Self::Application(l0, l1), Self::Application(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::ForAll(l0, l1), Self::ForAll(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Membership(l0, l1), Self::Membership(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::None, Self::None) => true,
            (Self::Known(s1), Self::Known(s2)) => s1 == s2,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Statement {
    Assigment(String, Expr),
    Print(Expr),
    Display(Expr),
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assigment(name, expr) => write!(f, "{} = {:?}", name, expr),
            Statement::Print(expr) => write!(f, "print {:?}", expr),
            Statement::Display(expr) => write!(f, "display {:?}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

fn preprocess_input(input: &str) -> String {
    return input.replace(",", " ,")
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = preprocess_input(input).chars().collect();
        let current_char = chars.get(0).copied();

        Lexer { input: chars, position: 0, current_char }
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
    fn skip_comment(&mut self) {
        if self.current_char == Some('#') {
            while let Some(ch) = self.current_char {
                if ch == '\n' {
                    break;
                }
                self.advance();
            }
        }
    }

    
    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.current_char {
            const BANNED_CHARS: &str = ";(){}[]@#=";

            // TODO: More checks on ch needed?
            if !ch.is_whitespace() && !BANNED_CHARS.contains(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();

            if self.current_char == Some('#') {
                self.skip_comment();
                continue;
            }

            match self.current_char {
                None => {return Token::Eof},

                Some('=') => {
                    self.advance();
                    return Token::Equals
                },
                Some('{') => {
                    self.advance();
                    return Token::LeftBrace;
                }
                Some('}') => {
                    self.advance();
                    return Token::RightBrace;
                }
                Some('(') => {
                    self.advance();
                    return Token::LeftParen;
                }
                Some(')') => {
                    self.advance();
                    return Token::RightParen;
                }
                Some(';') => {
                    self.advance();
                    return Token::Semicolon;
                }
                Some('@') => {
                    self.advance();
                    return Token::Apply;
                }
                Some(_) => {
                    let id = self.read_identifier();

                    return match id.as_str() {
                        "in" => {Token::In},
                        "none" => {Token::None},
                        "print" => {Token::Print},
                        "display" => {Token::Display},
                        "for" => {
                            self.skip_whitespace();

                            if self.current_char == Some('a') {
                                let next_word = self.read_identifier();
                                if next_word == "all" {
                                    Token::ForAll
                                } else {
                                    // Put back the word
                                    self.position -= next_word.len();
                                    self.current_char = self.input.get(self.position).copied();
                                    Token::Identifier("for".to_string())
                                }
                            } else {
                                Token::Identifier("for".to_string())
                            }
                        },
                        _ => {Token::Identifier(id)}
                    }
                }
            }
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Parser { lexer, current_token }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    /// Asserts that the current token is equal to the expected token, then advances
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", expected, self.current_token))
        }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();

        while self.current_token != Token::Eof {
            statements.push(self.parse_statement()?);
            log::debug!("current_tok: {:?} | statements: {:?}", self.current_token, statements);
            log::info!("statement finished: {:?}", statements[statements.len() - 1]);
        }

        Ok(Program { statements })
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match &self.current_token {
            Token::Print => {
                self.advance();
                let expr = self.parse_expression()?;
                Ok(Statement::Print(expr))
            },
            Token::Display => {
                self.advance();
                let expr = self.parse_expression()?;
                Ok(Statement::Display(expr))
            },
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();

                self.expect(Token::Equals)?;

                let expr = self.parse_expression()?;
                Ok(Statement::Assigment(name, expr))
            },
            _ => {
                Err(format!("Unable to parse statement type.\nID: {:?}", self.current_token))
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        
        let mut tokens = Vec::new();
        while self.current_token != Token::Semicolon {
            tokens.push(self.current_token.clone());
            self.advance();
            
            if self.current_token == Token::Eof {
                return Err("Missing Semicolon".to_string());
            }
        }
        self.advance();

        log::info!("Tokens in expression: {:?}", tokens);

        Self::parse_expression_from_tokens(tokens)
    }

    fn parse_expression_from_tokens_to_subexpressions(tokens: Vec<Token>) -> Result<Vec<Expr>, String> {
        const BRACKETS: [(Token, Token); 2] = [(Token::LeftParen, Token::RightParen), (Token::LeftBrace, Token::RightBrace)];

        log::debug!("Input Tokens for parsing: {:?}", tokens);

        let mut subexprs: Vec<Expr> = Vec::new();

        let mut application_next = false;
        let mut membership_next = false;

        let mut bracket_stack: Vec<Token> = Vec::new();
        let mut subexpr = Vec::new();
        for t in tokens.clone() {

            // Handle Brackets
            if BRACKETS.iter().any(|i| t == i.0) {
                bracket_stack.push(t.clone());
            } else if let Some(expected) = BRACKETS.iter().find(|i| t == i.1) {
                let popped = bracket_stack.pop();
                
                log::debug!("Expected Bracket: {:?}, Stack: {:?}", expected, bracket_stack);
                
                if popped != Some(expected.0.clone()) {
                    return Err(format!("Brackets or Parens do not match.\nCurrent Expression:{:?}\nBracket Stack: {:?}", tokens, bracket_stack));
                }
                if bracket_stack.len() == 0 {
                    if expected.1 == Token::RightParen {
                        subexprs.push(Self::parse_expression_from_tokens(subexpr[1..].to_vec())?);
                    } else if expected.1 == Token::RightBrace {
                        if subexpr.len() > 2 && subexpr[subexpr.len() - 2] == Token::ForAll {
                            if let Token::Identifier(name) = subexpr[subexpr.len() - 1].clone() {
                                subexprs.push(
                                    Expr::ForAll(
                                        // THISS IS IT!!! THIS IS THE BUG I FOUND!!!!!!
                                        Box::new(
                                            Self::parse_expression_from_tokens(subexpr[1..(subexpr.len() - 2)].to_vec())?
                                        ),
                                        name
                                    )
                                );
                            } else {
                                return Err(format!("Last token of a ForAll set is not an identifier.\nExpression: {:?}\nVariable: {:?}", tokens, subexpr[subexpr.len() - 1]))
                            }
                        } else {
                            subexprs.push(
                                Expr::SetLiteral(
                                    Self::parse_expression_from_tokens_to_subexpressions(subexpr[1..].to_vec())?
                                )
                            )
                        }
                    }
                    subexpr = Vec::new();
                }
            }
            if bracket_stack.len() != 0 {
                subexpr.push(t.clone());
            } else {
                // Possible tokens are [identifier, application, in, none]

                if let Token::Identifier(name) = t.clone() {
                    subexprs.push(Expr::Identifier(name));
                } else if Token::None == t {
                    subexprs.push(Expr::None);
                }

                assert!(!(application_next && membership_next));
                
                if application_next {
                    let argument = subexprs.pop().ok_or_else(|| "No Valid Argument for Application.".to_string())?;
                    let function = subexprs.pop().ok_or_else(|| "No Valid Argument for Application.".to_string())?;
                    subexprs.push(Expr::Application(
                        Box::new(function),
                        Box::new(argument)
                    ));

                    application_next = false;
                }

                if membership_next {
                    let set = subexprs.pop().ok_or_else(|| "No Valid Element for Membership.".to_string())?;
                    let element = subexprs.pop().ok_or_else(|| "No Valid Set for Membership.".to_string())?;
                    subexprs.push(Expr::Membership(
                        Box::new(element),
                        Box::new(set)
                    ));

                    membership_next = false;
                }
                
                if Token::Apply == t {
                    application_next = true;
                } else if Token::In == t {
                    membership_next = true;
                }

            }
        }

        if application_next {
            return Err(format!("End of expression before Application had enough arguments.\nExpression: {:?}", tokens));
        }
        if membership_next {
            return Err(format!("End of expression before Membership had enough arguments.\nExpression: {:?}", tokens));
        }

        Ok(subexprs)
    }

    fn parse_expression_from_tokens(tokens: Vec<Token>) -> Result<Expr, String> {
        let subexprs = Self::parse_expression_from_tokens_to_subexpressions(tokens)?;
        
        let mut subexprsdq: VecDeque<Expr> = subexprs.into_iter().collect();

        while subexprsdq.len() != 1 {
            log::debug!("Subexprsdq: {:?}", subexprsdq);

            if subexprsdq.len() == 2 {
                // At this point, it could be inferred to be an application, so this should be a warning, but it's fine to leave it as an error for proper formatting
                return Err(format!("Expression could not be parsed due to ambiguity in application for only two expressions.\nExpression: {:?}", subexprsdq));
            }

            assert!(subexprsdq.len() >= 3);

            let arg1 = subexprsdq.pop_front().unwrap();
            let map = subexprsdq.pop_front().unwrap();
            let arg2 = subexprsdq.pop_front().unwrap();

            let application = Expr::Application(
                Box::new(
                    Expr::Application(
                        Box::new(map),
                        Box::new(arg1)
                    )
                ),
                Box::new(arg2)
            );

            subexprsdq.push_front(application);
        }

        return subexprsdq.pop_front().ok_or_else(|| "Parsing resulted in wrong number of subexpressions.".to_string())
    }

}