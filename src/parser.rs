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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    SetLiteral(Vec<Expr>),
    Application(Box<Expr>, Box<Expr>),
    ForAll(Box<Expr>, String),
    Membership(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assigment(String, Expr),
    Print(Expr),
    Display(Expr),
    Expression(Expr),  // Is this necesary???
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
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
            // TODO: More checks on ch needed?
            if !ch.is_whitespace() {
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
            println!("{:?} | {:?}", self.current_token, statements);
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
                let expr = self.parse_expression()?;
                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        
        let mut tokens = Vec::new();
        while self.current_token != Token::Semicolon {
            tokens.push(self.current_token.clone());
        }

        Self::parse_expression_from_tokens(tokens)
    }

    fn parse_expression_from_tokens_to_subexpressions(tokens: Vec<Token>) -> Result<Vec<Expr>, String> {
        const BRACKETS: [(Token, Token); 2] = [(Token::LeftParen, Token::RightParen), (Token::LeftBrace, Token::RightBrace)];

        let mut subexprs: Vec<Expr> = Vec::new();

        let mut application_next = false;
        let mut membership_next = false;

        let mut bracket_stack: Vec<Token> = Vec::new();
        let mut subexpr = Vec::new();
        for t in tokens {

            // Handle Brackets
            if BRACKETS.iter().any(|i| t == i.0) {
                bracket_stack.push(t.clone());
            }
            else if BRACKETS.iter().any(|i| t == i.1) {
                let popped = bracket_stack.pop();
                if popped != Some(t.clone()) {
                    return Err("Brackets or Parens do not match.".to_string());
                }
                if bracket_stack.len() == 0 {
                    if popped == Some(Token::RightParen) {
                        subexprs.push(Self::parse_expression_from_tokens(subexpr.clone())?);
                    } else if popped == Some(Token::RightBrace) {
                        if subexpr.len() > 2 && subexpr[subexpr.len() - 2] == Token::ForAll {
                            if let Token::Identifier(name) = subexpr[subexpr.len() - 1].clone() {
                                subexprs.push(
                                    Expr::ForAll(
                                        Box::new(Expr::SetLiteral(vec![
                                            Self::parse_expression_from_tokens(subexpr[..(subexpr.len() - 2)].to_vec())?
                                        ])),
                                        name
                                    )
                                );
                            } else {
                                return Err("Last token of a ForAll set is not an identifier".to_string())
                            }
                        } else {
                            subexprs.push(
                                Expr::SetLiteral(
                                    Self::parse_expression_from_tokens_to_subexpressions(subexpr)?
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
                    subexprs.push(Expr::Identifier("none".to_string()));
                }

                assert!(!(application_next && membership_next));
                
                if application_next {
                    let argument = subexprs.pop().ok_or_else(|| "No Valid Argument for Application.".to_string())?;
                    let function = subexprs.pop().ok_or_else(|| "No Valid Argument for Application.".to_string())?;
                    subexprs.push(Expr::Application(
                        Box::new(argument),
                        Box::new(function)
                    ));

                    application_next = false;
                }

                if membership_next {
                    let element = subexprs.pop().ok_or_else(|| "No Valid Element for Membership.".to_string())?;
                    let set = subexprs.pop().ok_or_else(|| "No Valid Set for Membership.".to_string())?;
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
            return Err("End of expression before Application had enough arguments".to_string());
        }
        if membership_next {
            return Err("End of expression before Membership had enough arguments".to_string());
        }

        Ok(subexprs)
    }

    fn parse_expression_from_tokens(tokens: Vec<Token>) -> Result<Expr, String> {
        let subexprs = Self::parse_expression_from_tokens_to_subexpressions(tokens)?;
        
        let mut subexprsdq: VecDeque<Expr> = subexprs.into_iter().collect();

        while subexprsdq.len() != 1 {
            if subexprsdq.len() == 2 {
                // At this point, it could be inferred to be an application, so this should be a warning, but it's fine to leave it as an error for proper formatting
                return Err("Expression could not be parsed due to ambiguity in application for only two expressions.".to_string());
            }

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

    // fn parse_expression(&mut self) -> Result<Expr, String> {
    //     let mut subexprs: Vec<Expr> = Vec::new();
    //     let mut subject_of_membership = false;
    //     let mut subject_of_function = false;

    //     while self.current_token != Token::Semicolon {
    //         if self.current_token == Token::LeftParen {

    //             // Parse Sub-Expression
    //             self.advance();
    //             subexprs.push(self.parse_expression()?);
    //             self.expect(Token::RightParen)?;

    //         } else if self.current_token == Token::LeftBrace {
                
    //             // Parse set
    //             self.advance();
    //             let mut elements: Vec<Expr> = Vec::new();
    //             while self.current_token != Token::RightBrace && self.current_token != Token::ForAll {
    //                 elements.push(self.parse_expression()?);
    //             }
                
    //             if self.current_token == Token::RightBrace {
    //                 // Set Literal
    //                 subexprs.push(Expr::SetLiteral(elements));
    //             } else {
    //                 // Set Comprehension
    //                 self.advance();
    //                 if elements.len() != 1 {
    //                     return Err("Non-singular element in set comprehension".to_string())
    //                 }
    //                 let Token::Identifier(name) = self.current_token.clone() else {return Err("No variable found in Set Comprehension".to_string())};
    //                 subexprs.push(Expr::ForAll(Box::new(elements[0].clone()), name));
    //             }
                
    //             self.expect(Token::RightBrace)?;

    //         } else if let Token::Identifier(name) = self.current_token.clone() {
    //             subexprs.push(Expr::Identifier(name));
    //             self.advance();
    //         } else if self.current_token == Token::Apply {
    //             subject_of_function = true;
    //             self.advance();
    //         } else if self.current_token == Token::In {
    //             subject_of_membership = true;
    //             self.advance();
    //         } else if self.current_token == Token::None {
    //             subexprs.push(Expr::Identifier("none".to_string()));
    //         } else {
    //             return Err("Incorrect token encountered during parsing expression.".to_string());
    //         }

    //         if subject_of_membership && !(self.current_token == Token::In) {
    //             let expr2 = subexprs.pop().unwrap();
    //             let expr1 = subexprs.pop().unwrap();
    //             subexprs.push(Expr::Membership(Box::new(expr1), Box::new(expr2)));
    //             subject_of_membership = false;
    //         }

    //         if subject_of_function && !(self.current_token == Token::Apply) {
    //             let value = subexprs.pop().unwrap();
    //             let func = subexprs.pop().unwrap();
    //             subexprs.push(Expr::Application(Box::new(func), Box::new(value)));
    //             subject_of_function = false;
    //         }
    //     }

    //     while subexprs.len() >= 3 {
    //         let expr1 = subexprs[0].clone();
    //         let func = subexprs[1].clone();
    //         let expr2 = subexprs[2].clone();

    //         let mut new_subexprs = vec![Expr::Application(Box::new(Expr::Application(Box::new(func), Box::new(expr1))), Box::new(expr2))];
    //         new_subexprs.extend_from_slice(&subexprs[3..]);

    //         subexprs = new_subexprs;
    //     }

    //     if subexprs.len() == 2 {
    //         return Err("2 Expressions encountered without an operator".to_string())
    //     }

    //     if subexprs.len() == 0 {
    //         return Ok(Expr::Identifier("none".to_string()));
    //     }

    //     return Ok(subexprs[0].clone());
    // }


}


use std::fmt;
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(name) => write!(f, "{}", name),
            Expr::SetLiteral(elements) => {
                write!(f, "{{")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "}}")
            }
            Expr::Application(func, arg) => write!(f, "{}({})", func, arg),
            Expr::ForAll(expr, var) => {write!(f, "{{{} for all {}}}", expr, var)}
            Expr::Membership(elem, set) => write!(f, "{} in {}", elem, set),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assigment(name, expr) => write!(f, "{} = {}", name, expr),
            Statement::Print(expr) => write!(f, "print {}", expr),
            Statement::Display(expr) => write!(f, "display {}", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}