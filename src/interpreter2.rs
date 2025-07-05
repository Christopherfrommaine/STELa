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
                self.values.insert(name, self.interpret_expression(expr)?);
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

    fn interpret_expression
}