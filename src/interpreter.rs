use crate::parser::{Expr, Statement};
use std::collections::{BTreeSet, HashMap};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Value {
    None,                      // None value
    Known(Set),                // Exactly known structure
    Literal(BTreeSet<Value>),  // Known values but subvalues can be anything
    Unknown(Expr),             // Infinite, generator, or unknown
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => {write!(f, "None")},
            Value::Literal(vec) => {
                write!(f, "{:?}", vec.iter().collect::<Vec<&Value>>())
            },
            Value::Known(set) => {
                write!(f, "{{{}}}", set.s.iter().map(|i| format!("{:?}", i)).collect::<Vec<String>>().join(", "))
            },
            Value::Unknown(expr) => {write!(f, "{:?}", expr)}
        }
    }
}

impl Value {
    fn to_set(&self) -> Result<Set, String> {
        match self {
            Value::Known(s) => {Ok(s.clone())},
            Value::Literal(s) => {
                Ok(Set::new(s.iter().map(|i| i.to_set()).collect::<Result<BTreeSet<Set>, String>>()?))
            },
            _ => {Err(format!(".to_set() method used on unknown or non-set value."))}
        }
    }
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set {
    s: BTreeSet<Set>,
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
    values: HashMap<String, Value>,
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

    pub fn advance(&mut self) -> Result<(), String> {
        match self.current_statement().clone() {
            Statement::Assigment(name, expr) => {
                self.values.insert(name, self.interpret_expression(expr)?);
            },
            Statement::Print(expr) => {
                todo!();
            },
            Statement::Display(expr) => {
                todo!();
            },
        }

        self.position += 1;
        
        if self.position >= self.ast.len() {
            return Err(format!("Program Complete"));
        }

        Ok(())
    }

    fn interpret_expression(&self, expr: Expr) -> Result<Value, String> {
        match expr {
            Expr::Identifier(name) => {
                if name == "none" {
                    Ok(Value::None)
                } else {
                    self.values.get(&name).cloned().ok_or_else(|| format!("Variable Name Not Found.\nExpr: {:?}", name))
                }
            },
            Expr::SetLiteral(exprs) => {
                Ok(Value::Literal(
                    exprs.into_iter()
                    .map(|expr| self.interpret_expression(expr))
                    .collect::<Result<BTreeSet<Value>, String>>()?
                ))
            },
            Expr::Application(func, expr) => {
                Ok(Value::Unknown(Expr::Application(func, expr)))
            },
            Expr::ForAll(expr, name) => {
                Ok(Value::Unknown(Expr::ForAll(expr, name)))
            },
            Expr::Membership(elem, set) => {
                Ok(Value::Unknown(Expr::Membership(elem, set)))
            },
        }
    }

    fn simplify_value(&self, value: Value, depth: u32) -> Result<Value, String> {
        if depth == 0 {
            return Err(format!("Max Recursion Depth of Exceeded. Please look for self-refferential formulae."));
        }

        match value {
            Value::None => {Ok(value)},
            Value::Known(s) => {Ok(Value::Known(s))},
            Value::Literal(s) => {
                if let Ok(s) = Value::Literal(s.clone()).to_set() {
                    Ok(Value::Known(s))
                } else {
                    Ok(Value::Literal(
                        s.into_iter()
                        .map(|i|
                            self.simplify_value(i, depth - 1)
                        ).collect::<Result<BTreeSet<Value>, String>>()?
                    ))
                }
            },
            Value::Unknown(expr) => {
                match expr {
                    Expr::Identifier(name) => {
                        self.values.get(&name).cloned().ok_or_else(|| format!("Variable Name Not Found.\nExpr: {:?}", name))
                    },
                    Expr::SetLiteral(exprs) => {
                        Ok(Value::Literal(
                            exprs
                            .into_iter()
                            .map(|i|
                                self.simplify_value(
                                    self.interpret_expression(i)?, depth - 1
                                )
                            )
                            .collect::<Result<BTreeSet<Value>, String>>()?
                        ))
                    },
                    Expr::Application(func, arg) => {
                        // HERE!
                        // I'll have to do some sort of pattern matching?
                        // A func is either a Expr::ForAll or Expr::SetLiteral.
                        // For a Expr::SetLiteral, linearly search through tuples (which are sets of the form {{a} {a b}}) for if the a equals the argument.
                        // For a Expr::ForAll, use pattern matching to find a value for the name which makes the ForAll's expression match the argument
                        match *func {
                            Expr::SetLiteral(set) => {
                                for s in set {
                                    if let Ok((inp, out)) = s.destructure_tuple(){
                                        if inp == *arg {
                                            return Ok(Value::Unknown(out));
                                        }
                                    }
                                }

                                Err(format!("No Matching Input Found in SetLiteral Function Application."))
                            },
                            Expr::ForAll(expr, name) => {
                                // Screw pattern matching
                                if let Ok((t1, t2)) = expr.destructure_tuple() {
                                    if t1 == Expr::Identifier(name.clone()) {
                                        Ok(Value::Unknown(t2.substitute(name, *arg)?))
                                    } else {
                                        Err(format!("Pattern matching not supported at this time."))
                                    }
                                } else {
                                    Err(format!("Unable to interpret ForAll expression as a tuple map."))
                                }
                            },
                            _ => {Err(format!("Function is not of a form which can be interpreted as a function."))}
                        }
                    },
                    Expr::ForAll(expr, name) => {
                        Ok(Value::Unknown(Expr::ForAll(expr, name)))
                    },
                    Expr::Membership(elem, set) => {
                        todo!()
                    },
                }

            },
        }
    }

    fn set_value(&self, value: Value) -> Result<Option<Set>, String> {
        let mut v = value;
        let mut prev = Value::None;
        while v != prev {
            prev = v.clone();
            v = self.simplify_value(v, 255)?;
        }

        if let Value::Known(s) = v {
            Ok(Some(s))
        } else {
            Ok(None)
        }
    }
}

impl Expr {
    fn substitute(self, name: String, value: Expr) -> Result<Expr, String> {
        match self.clone() {
            Expr::Identifier(other) => {Ok(if name == other {value} else {self})},
            Expr::SetLiteral(exprs) => {Ok(Expr::SetLiteral(exprs.into_iter().map(|i| i.substitute(name.clone(), value.clone())).collect::<Result<Vec<Expr>, String>>()?))},
            Expr::Application(func, arg) => {Ok(Expr::Application(Box::new(func.substitute(name.clone(), value.clone())?), Box::new(arg.substitute(name.clone(), value.clone())?)))},
            Expr::ForAll(expr, other) => {if name == other {Err(format!("Variable Collision"))} else {Ok(Expr::ForAll(Box::new(expr.substitute(name, value)?), other))}},
            Expr::Membership(elem, set) => {Ok(Expr::Membership(Box::new(elem.substitute(name.clone(), value.clone())?), Box::new(set.substitute(name, value)?)))},
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
