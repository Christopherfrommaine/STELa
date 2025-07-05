use crate::parser::{Expr, Statement};
use std::collections::{BTreeSet, HashMap};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Value {
    None,                      // None value
    Known(Set),                // Exactly known structure
    Literal(BTreeSet<Value>),  // Known values but subvalues can be anything
    Unknown(Expr),             // Infinite, generator, or unknown
}

impl Value {
    fn all_known(&self) -> bool {
        match self {
            Value::None => {false},
            Value::Known(_) => {true},
            Value::Literal(s) => {s.iter().all(|i| i.all_known())},
            Value::Unknown(_) => {false},
        }
    }

    fn to_set(&self) -> Set {
        assert!(self.all_known());
        
        let mut o = BTreeSet::new();
        if let Value::Literal(s) = self {
            o = s.iter().map(|i| i.to_set()).collect();
        }
        
        Set::new(o)
    }

    fn to_known(&self) -> Self {
        Value::Known(self.to_set())
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


struct Interpreter {
    ast: Vec<Statement>,
    values: HashMap<String, Value>,
    position: usize,
}

impl Interpreter {
    fn new(ast: Vec<Statement>) -> Self {
        Interpreter {
            ast,
            values: HashMap::new(),
            position: 0,
        }
    }

    fn current_statement(&self) -> Statement {
        self.ast[self.position].clone()
    }

    fn advance(&mut self) -> Result<(), String> {
        match self.current_statement().clone() {
            Statement::Assigment(name, expr) => {
                self.values.insert(name, self.lazy_eval_expr(expr)?);
            },
            Statement::Print(expr) => {
                println!("{:?}", self.must_eval(expr));
            },
            Statement::Display(expr) => {
                todo!();
            },
        }

        Ok(())
    }

    fn lazy_eval(&self, val: Value) -> Result<Value, String> {
        match val {
            Value::None => {Ok(val)},
            Value::Known(_) => {Ok(val)},
            Value::Literal(s) => {
                let o = Value::Literal(
                    elems.into_iter()
                    .map(|i| self.lazy_eval_expr(i))
                    .collect()?
                );

                if o.all_known() {
                    Ok(o.to_known())
                } else {
                    Ok(o)
                }
            },
            Value::Unknown(s) => {
                self.lazy_eval_expr(s)
            },
        }
    }
    
    fn lazy_eval_expr(&self, expr: Expr) -> Result<Value, String> {
        match expr {
            Expr::Identifier(name) => {
                self.values.get(&name).cloned().ok_or_else(|| format!("Use of unassigned variable.\nVariable: {}", name))
            },
            Expr::SetLiteral(elems) => {
                let o = Value::Literal(
                    elems.into_iter()
                    .map(|i| self.lazy_eval_expr(i))
                    .collect()?
                );

                if o.all_known() {
                    Ok(o.to_known())
                } else {
                    Ok(o)
                }
            },
            Expr::ForAll(expr, var) => {
                Ok(Value::Unknown(Expr::ForAll(expr, var)))
            },
            Expr::Membership(elem, set) => {

                match (self.lazy_eval_expr(*set)?, self.lazy_eval_expr(*elem)?) {
                    (Value::Known(s), Value::Known(e)) => {
                        Ok(Value::Known(Set::from_bool(s.s.contains(&e))))
                    },
                    (Value::Known(s), Value::Literal(e)) => {
                        Ok(Value::Known(Set::from_bool(s.contains(&Value::Literal(e)))))
                    },
                    (Value::Known(s), Value::None) => {
                        eprintln!("Warning: Please don't use none values.");
                        Ok(Value::Known(Set::one()))
                    },
                    (Value::Known(s), Value::Unknown(e)) => {
                        Ok(Value::Known(Set::from_bool(s.contains(&Value::Unknown(e)))))
                    },

                    (Value::Literal(s), Value::Known(e)) => {
                        Ok(Value::Known(Set::from_bool(s.contains(&Value::Known(e)))))
                    },
                    (Value::Literal(s), Value::Literal(e)) => {
                        Ok(Value::Known(Set::from_bool(s.contains(&Value::Literal(e)))))
                    },
                    (Value::Literal(s), Value::None) => {
                        eprintln!("Warning: Please don't use none values.");
                        Ok(Value::Known(Set::one()))
                    },
                    (Value::Literal(s), Value::Unknown(e)) => {
                        Ok(Value::Known(Set::from_bool(s.contains(&Value::Unknown(e)))))
                    },

                    (_, _) => {todo!();},
                }

            },
            Expr::Application(func, arg) => {
                todo!()
            },
        }
    }

    fn exact_eval(&self, expr: Expr) -> Option<Set> {
        todo!()
    }

    fn must_eval(&self, expr: Expr) -> Set {
        todo!();
    }
}