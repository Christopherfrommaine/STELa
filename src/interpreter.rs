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

    fn contains_membership_checks_of(&self) -> Vec<Expr> {

        let mut o = Vec::new();
        
        match self {
            Expr::SetLiteral(exprs) => {
                o.extend(exprs.into_iter().flat_map(|i| i.contains_membership_checks_of()))
            },
            Expr::Membership(_elem, set) => {
                o.push(*set.clone());
            },
            Expr::Application(_func, arg) =>{
                o.extend(arg.contains_membership_checks_of());
            },
            _ => {},
        }

        o
    }

    fn format_tree(&self) -> String {
        crate::format_tree::format_tree(&format!("{self:?}"))
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

    fn to_literal(&self) -> Expr {
        Expr::SetLiteral(self.s.iter().map(|i| i.to_literal()).collect())
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
            if self.values.contains_key("prelude_finished") {
                print!("");
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
                let ex = self.evaluate(expr, false)?;
                println!("VS: {} = {:?}", name, ex);
                self.values.insert(name, ex);
            },
            Statement::Print(expr) => {
                match self.force_evaluate(expr.clone())? {
                    Some(o) => {out += &format!("\nPRNT: {:?}\n", o);},
                    None => {out += &format!("\nPRNT: {:?}\n", expr);}
                }
                
            },
            Statement::Display(expr) => {
                let o = self.force_evaluate(expr.clone())?.ok_or_else(|| format!("Unable to find exact set value of string.\nexpr: {:?}\nevaled expr: {:?}", expr.clone(), self.evaluate(expr, false)))?;
                let s = o.interpret_as_string()?;
                out += &format!("\nDISP: {}\n", s);
            },
        }

        print!("{out}");
        self.stdout += &out;


        self.position += 1;

        Ok(())
    }

    fn evaluate(&self, expr: Expr, forced: bool) -> Result<Expr, String> {
        println!("e: {:?}", expr);

        match expr {
            Expr::None => {Ok(Expr::None)},
            Expr::Known(s) => {Ok(Expr::Known(s))}
            Expr::Identifier(name) => {
                match self.values.get(&name).cloned() {
                    Some(value) => {self.evaluate(value, forced)},
                    None => {Ok(Expr::Identifier(name))},
                }
            },
            Expr::ForAll(expr, name) => {


                if !forced {
                    return Ok(Expr::ForAll(Box::new(self.evaluate(*expr, forced)?), name))
                }

                eprintln!("USING BACKUP FORALL EVALUATION SYSTEM.");

                let members = expr.contains_membership_checks_of();
                println!("Members: {:?}", members);
                if members.len() == 0 {
                    return Err(format!("Unable to interpret ForAll expression as a tuple map.\nExpr: {:?}", *expr));
                }

                let mut all_elements = Vec::new();

                for member in members {
                    all_elements.extend(self.force_evaluate(member)?.ok_or(format!("Ruh roh"))?.s.into_iter());
                }

                let mut o = Vec::new();

                for e in all_elements {
                    o.push(expr.clone().substitute(name.clone(), Expr::Known(e))?);
                }

                self.evaluate(Expr::SetLiteral(o), forced)
            },
            Expr::SetLiteral(exprs) => {
                if let Ok(set) = Expr::SetLiteral(exprs.clone()).to_set() {
                    Ok(Expr::Known(set))
                } else {
                    let o = Ok(Expr::SetLiteral(exprs.clone().into_iter().map(|i| self.evaluate(i, forced)).collect::<Result<Vec<Expr>, String>>()?));
                    println!("a reminder: e = {:?}", exprs);
                    
                    if let Ok(o1) = o {
                        if let Ok(set) = o1.to_set() {
                            Ok(Expr::Known(set))
                        } else {
                            Ok(o1)
                        }
                    } else {
                        o
                    }
                }
            },
            Expr::Application(func, arg) => {
                match *func {
                    Expr::SetLiteral(set) => {
                        for s in set.clone() {
                            if let Ok((inp, out)) = s.destructure_tuple(){
                                if inp == *arg {
                                    return Ok(out);
                                }
                            }
                        }

                        Err(format!("No Matching Input Found in SetLiteral Function Application.\nExpr: {:?}", set))
                    },
                    Expr::Known(set) => {
                        for s in set.s {
                            if let Ok((inp, out)) = Expr::Known(s).destructure_tuple(){
                                if inp == *arg {
                                    return Ok(out);
                                }
                            }
                        }

                        Err(format!("No Matching Input Found in KnownSet Function Application."))
                    },
                    Expr::ForAll(expr, name) => {
                        // Screw pattern matching
                        match expr.clone().destructure_tuple() {
                            Ok((t1, t2)) => {
                                if t1 == Expr::Identifier(name.clone()) {
                                    let o = t2.substitute(name, *arg);
                                    println!("ooo {:?}", o);
                                    o
                                } else {
                                    Err(format!("Pattern matching not supported at this time."))
                                }
                            }
                            Err(e) => {
                                Err(format!("Unable to interpret ForAll expression as a tuple map.\nExpr: {:?}\nErr: {}", *expr, e))
                            }
                        }
                    },
                    _ => {
                        // Err(format!("Function is not of a form which can be interpreted as a function.\nFunc: {:?}", func))
                        self.evaluate(Expr::Application(
                            Box::new(self.evaluate(*func, false)?),
                            Box::new(self.evaluate(*arg,  false)?),
                        ), forced)
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
            expr = self.evaluate(expr, true)?;
        }
        
        if let Expr::Known(s) = expr {
            Ok(Some(s))
        } else {
            println!("Unable to force evaluation.\nMost simplified form: {:?}", expr);
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

        if let Expr::Known(s) = self {
            return s.to_literal().destructure_tuple();
        }

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

                    println!("Destructuring sucuessful! ({:?}, {:?})", tuple0, tuple1);
                    Ok((tuple0, tuple1))
                } else {
                    Err(format!("Cannot be interpreted as tuple. Unable to find two set literals."))
                }
            } else {
                Err(format!("Set literal contains wrong number of elements ({}) to be interpreted as a pair.", exprs.len()))
            }
        } else {
            Err(format!("Wrong expression type to be destructured"))
        }
    }
}
