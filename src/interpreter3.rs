use crate::parser::{Expr, Statement};
use std::collections::{BTreeSet, HashMap};

impl Expr {
    fn to_set(&self) -> Result<Set, String> {
        match self {
            Expr::Known(set) => {Ok(set.clone())},
            Expr::SetLiteral(exprs) => {Ok(Set::new(exprs.iter().cloned().map(|i| i.to_set()).collect::<Result<BTreeSet<Set>, String>>()?))},
            _ => {Err(format!("Expr could not be converted to a set."))}
        }
    }
}


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set {
    s: BTreeSet<Set>,
}

impl std::fmt::Debug for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.s)
    }
}

impl Set {
    fn new(s: BTreeSet<Set>) -> Self {
        Set { s }
    }

    fn zero() -> Set {
        Set::new(BTreeSet::new())
    }

    fn one() -> Set {
        Set::new({let mut o = BTreeSet::new(); o.insert(Set::zero()); o})
    }

    fn from_bool(b: bool) -> Set {
        if b {
            Self::one()
        } else {
            Self::zero()
        }
    }

    fn ackermann(&self) -> u64 {
        self.s.iter().map(|i| 1 << i.ackermann()).sum()
    }
}

#[derive(Debug)]
pub struct Interpreter {
    ast: Vec<Statement>,
    values: HashMap<String, Expr>,
    position: usize,
}

impl Interpreter {
    pub fn new(ast: Vec<Statement>) -> Self {
        Interpreter {
            ast,
            values: HashMap::new(),
            position: 0,
        }
    }

    fn current_statement(&self) -> Statement {
        self.ast[self.position].clone()
    }

    pub fn run(&mut self) -> Result<(), String> {
        while self.position < self.ast.len() {
            if let Statement::Assigment(name, _) = self.current_statement().clone() {
                if name == "s".to_string() {
                    print!("");
                }
            }

            self.advance()?;
        }

        Ok(())
    }

    pub fn advance(&mut self) -> Result<(), String> {
        println!("C: {:?}", self.current_statement());

        match self.current_statement().clone() {
            Statement::Assigment(name, expr) => {
                self.values.insert(name, expr);
            },
            Statement::Print(expr) => {
                println!("PRINT: {:?}", self.force_evaluate(expr)?.ok_or(format!("Unable to find exact set value of string."))?);
                // todo!();
            },
            Statement::Display(expr) => {
                todo!();
            },
        }

        self.position += 1;

        Ok(())
    }

    fn evaluate(&self, expr: Expr) -> Result<Expr, String> {

        match expr {
            Expr::None => {Ok(Expr::None)},
            Expr::Known(s) => {Ok(Expr::Known(s))}
            Expr::Identifier(name) => {self.evaluate(self.values.get(&name).cloned().ok_or(format!("Use of undeclared variable."))?)},
            Expr::ForAll(expr, name) => {Ok(Expr::ForAll(Box::new(self.evaluate(*expr)?), name))},
            Expr::SetLiteral(exprs) => {
                if let Ok(set) = Expr::SetLiteral(exprs.clone()).to_set() {
                    Ok(Expr::Known(set))
                } else {
                    Ok(Expr::SetLiteral(exprs.into_iter().map(|i| self.evaluate(i)).collect::<Result<Vec<Expr>, String>>()?))
                }
            },
            Expr::Application(func, arg) => {
                match *func {
                    Expr::SetLiteral(set) => {
                        for s in set {
                            if let Ok((inp, out)) = s.destructure_tuple(){
                                if inp == *arg {
                                    return Ok(out);
                                }
                            }
                        }

                        Err(format!("No Matching Input Found in SetLiteral Function Application."))
                    },
                    Expr::ForAll(expr, name) => {
                        // Screw pattern matching
                        if let Ok((t1, t2)) = expr.destructure_tuple() {
                            if t1 == Expr::Identifier(name.clone()) {
                                t2.substitute(name, *arg)
                            } else {
                                Err(format!("Pattern matching not supported at this time."))
                            }
                        } else {
                            Err(format!("Unable to interpret ForAll expression as a tuple map."))
                        }
                    },
                    _ => {
                        // Err(format!("Function is not of a form which can be interpreted as a function.\nFunc: {:?}", func))
                        Ok(
                            Expr::Application(
                                Box::new(self.evaluate(*func)?),
                                arg
                            )
                        )
                    }
                }
            },
            Expr::Membership(elem, set) => todo!(),
        }
    }

    fn force_evaluate(&self, expr: Expr) -> Result<Option<Set>, String> {
        if let Expr::Known(s) = self.evaluate(expr)? {
            Ok(Some(s))
        } else {
            Ok(None)
        }
    }
}

impl Expr {
    fn substitute(self, name: String, value: Expr) -> Result<Expr, String> {
        match self {
            Expr::Identifier(other) => {Ok(if name == other {value} else {Expr::Identifier(other)})},
            Expr::SetLiteral(exprs) => {Ok(Expr::SetLiteral(exprs.into_iter().map(|i| i.substitute(name.clone(), value.clone())).collect::<Result<Vec<Expr>, String>>()?))},
            Expr::Application(func, arg) => {Ok(Expr::Application(Box::new(func.substitute(name.clone(), value.clone())?), Box::new(arg.substitute(name.clone(), value.clone())?)))},
            Expr::ForAll(expr, other) => {if name == other {Err(format!("Variable Collision"))} else {Ok(Expr::ForAll(Box::new(expr.substitute(name, value)?), other))}},
            Expr::Membership(elem, set) => {Ok(Expr::Membership(Box::new(elem.substitute(name.clone(), value.clone())?), Box::new(set.substitute(name, value)?)))},
            Expr::Known(set) => {Ok(Expr::Known(set))},
            Expr::None => {Ok(Expr::None)}
        }
    }

    fn destructure_tuple(self) -> Result<(Expr, Expr), String> {

        if let Expr::SetLiteral(exprs) = self {
            if exprs.len() == 2 {
                if let (Expr::SetLiteral(e1), Expr::SetLiteral(e2)) = (exprs[0].clone(), exprs[1].clone()) {
                    let smaller: Vec<Expr>;
                    let larger: Vec<Expr>;

                    match (e1.len(), e2.len()) {
                        (1, 2) => {
                            (smaller, larger) = (e1, e2);
                        },
                        (2, 1) => {
                            (smaller, larger) = (e2, e1);
                        },
                        _ => {
                            return Err(format!("Cannot be interpreted as tuple. Wrong number of subelements."));
                        }
                    }

                    let tuple0 = smaller[0].clone();
                    let tuple1 = if tuple0 == larger[0] {larger[1].clone()} else {larger[0].clone()};
                    // Remember to check for the case of {{a} {a a}}

                    Ok((tuple0, tuple1))
                } else {
                    Err(format!("Cannot be interpreted as tuple. Unable to find two set literals."))
                }
            } else {
                Err(format!("Set literal contains too many elements to be interpreted as a pair."))
            }
        } else {
            Err(format!("Wrong expression type to be destructured"))
        }
    }
}
