use crate::parser::{Expr, Statement};
use std::{collections::{BTreeSet, HashMap}, hash::Hash, string::FromUtf8Error, vec};

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

    fn ackermann(&self) -> u32 {
        self.s.iter().map(|i| 1 << i.ackermann()).sum()
    }

    fn interpret_as_integer(&self) -> Result<u32, String> {
        if self.s.len() == 0 {
            Ok(0)
        } else {
            let mut o = self.s.iter().map(|ss| ss.interpret_as_integer()).collect::<Result<Vec<u32>, String>>()?;
            o.sort_unstable();

            let mx = *o.iter().max().unwrap();
            
            if o != (0..=mx).collect::<Vec<u32>>() {
                return Err(format!("Set could not be converted to integer. Incorrect Structure: {:?}", o));
            }

            Ok(mx + 1)
        }
    }

    fn interpret_as_vector(&self) -> Result<Vec<Set>, String> {
        let mut vector_values = vec![None; self.s.len()];

        for ss in self.s.clone() {
            let (t1, t2) = Expr::Known(ss).destructure_tuple()?;
            
            if let Expr::Known(set) = t1 {
                let index = set.interpret_as_integer()? as usize;

                if index >= self.s.len() {
                    return Err(format!("Index out of range during vector interpretation."))
                }

                vector_values[index] = Some(t2);

            } else {
                return Err(format!("Index is not a known set."));
            }
        }

        let vector_values_as_option = vector_values.into_iter().collect::<Option<Vec<Expr>>>();

        if let Some(o) = vector_values_as_option {
            Ok(o.into_iter().map(|expr| match expr {
                Expr::Known(s) => s,
                _ => unreachable!(),
            }).collect())
        } else {
            Err(format!("Some indices are missing from the vector."))
        }
    }

    fn interpret_as_string(&self) -> Result<String, String> {
        let vs = self
            .interpret_as_vector()?
            .into_iter()
            .map(|i| i.ackermann())
            .map(|a| if a <= 255 {Ok(a as u8)} else {Err(format!("Integer out of bounds for interpretation as UTF-8"))})
            .collect::<Result<Vec<u8>, String>>()?;

        match String::from_utf8(vs) {
            Err(_) => {Err(format!("Byte Array not valid UTF-8"))},
            Ok(string) => {Ok(string)}
        }

    }
}

#[derive(Debug)]
pub struct Interpreter {
    ast: Vec<Statement>,
    values: HashMap<String, Expr>,
    position: usize,
    stdout: String,
}

impl Interpreter {
    pub fn new(ast: Vec<Statement>) -> Self {
        let mut values = HashMap::new();
        // , = {(a, {(b, {{a} {a b}}) for all b}) for all a}
        let arg1 = "arg1".to_string();
        let arg2 = "arg2".to_string();
        let arg1id = Expr::Identifier(arg1.clone());
        let arg2id = Expr::Identifier(arg2.clone());
        

        let tuple = |a: Expr, b: Expr| Expr::SetLiteral(vec![Expr::SetLiteral(vec![a.clone()]), Expr::SetLiteral(vec![a, b])]);
        let boxed_tuple = |a: Expr, b: Expr| Box::new(tuple(a, b));

        let tuple_generator =
        Expr::ForAll(
            boxed_tuple(
                arg1id.clone(),
                Expr::ForAll(
                    boxed_tuple(
                        arg2id.clone(),
                        tuple(arg1id, arg2id)
                    ),
                    arg2
                ),
            ),
            arg1
        );

        println!("Tuple Gen: {:?}", tuple_generator);
        
        values.insert(",".to_string(), tuple_generator);

        Interpreter {
            ast,
            values,
            position: 0,
            stdout: String::new(),
        }
    }

    fn current_statement(&self) -> Statement {
        self.ast[self.position].clone()
    }

    pub fn run(&mut self) -> Result<String, String> {
        while self.position < self.ast.len() {
            if let Statement::Assigment(name, _) = self.current_statement().clone() {
                if name == "s".to_string() {
                    print!("");
                }
            }

            self.advance()?;
        }

        Ok(self.stdout.clone())
    }

    pub fn advance(&mut self) -> Result<(), String> {
        println!("C: {:?}", self.current_statement());

        let mut out = String::new();

        match self.current_statement().clone() {
            Statement::Assigment(name, expr) => {
                self.values.insert(name, expr);
            },
            Statement::Print(expr) => {
                let o = self.force_evaluate(expr.clone())?.ok_or(format!("Unable to find exact set value of string.\nexpr: {:?}\nevaled expr: {:?}", expr.clone(), self.evaluate(expr)))?;
                out += &format!("\nPRNT: {:?}\n", o);
            },
            Statement::Display(expr) => {
                let o = self.force_evaluate(expr.clone())?.ok_or(format!("Unable to find exact set value of string.\nexpr: {:?}\nevaled expr: {:?}", expr.clone(), self.evaluate(expr)))?;
                let s = o.interpret_as_string()?;
                out += &format!("\nDISP: {}\n", s);
            },
        }

        print!("{out}");
        self.stdout += &out;


        self.position += 1;

        Ok(())
    }

    fn evaluate(&self, expr: Expr) -> Result<Expr, String> {
        println!("e: {:?}", expr);

        match expr {
            Expr::None => {Ok(Expr::None)},
            Expr::Known(s) => {Ok(Expr::Known(s))}
            Expr::Identifier(name) => {
                match self.values.get(&name).cloned() {
                    Some(value) => {self.evaluate(value)},
                    None => {Ok(Expr::Identifier(name))},
                }
            },
            Expr::ForAll(expr, name) => {Ok(Expr::ForAll(Box::new(self.evaluate(*expr.clone()).unwrap_or(*expr)), name))},
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
                        self.evaluate(Expr::Application(
                            Box::new(self.evaluate(*func)?),
                        arg
                        ))
                        
                    }
                }
            },
            Expr::Membership(elem, set) => todo!(),
        }
    }

    fn force_evaluate(&self, original: Expr) -> Result<Option<Set>, String> {
        let mut expr = original;
        let mut prev = Expr::None;
        while !prev.eq(&expr) {
            println!("P,C: {:?} | {:?}", prev, expr);
            prev = expr.clone();
            expr = self.evaluate(expr)?;
        }
        
        if let Expr::Known(s) = expr {
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

        println!("Destructuring: {:?}", self);

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
