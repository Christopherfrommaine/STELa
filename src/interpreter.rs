use crate::parser::{Expr, Statement};
use std::{collections::{BTreeSet, HashMap}, hash::Hash, vec};

impl Expr {
    fn to_set(&self) -> Result<Set, String> {
        match self {
            Expr::Known(set) => {Ok(set.clone())},
            Expr::SetLiteral(exprs) => {Ok(Set::new(exprs.iter().cloned().map(|i| i.to_set()).collect::<Result<BTreeSet<Set>, String>>()?))},
            _ => {Err(format!("Expr could not be converted to a set."))}
        }
    }

    /// Transforms any Expr::Known into Expr::Literal. e.g. for tuple destructuring
    fn to_literal(self) -> Expr {
        match self {
            Expr::Known(set) => {set.to_literal()},
            Expr::SetLiteral(exprs) => {Expr::SetLiteral(exprs.into_iter().map(|i| i.to_literal()).collect())},
            expr => {expr}
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
            Expr::Application(func, arg) =>{
                o.extend(func.contains_membership_checks_of());
                o.extend(arg.contains_membership_checks_of());
            },
            _ => {},
        }

        o
    }

    #[allow(dead_code)]
    fn format_tree(&self) -> String {
        crate::format_tree::format_tree(&format!("{self:?}"))
    }

    fn zero() -> Expr {
        Expr::Known(Set::zero())
    }

    fn one() -> Expr {
        Expr::Known(Set::one())
    }

    fn from_bool(b: bool) -> Expr {
        Expr::Known(Set::from_bool(b))
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

    #[allow(dead_code)]
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
        let arg1 = "a".to_string();
        let arg2 = "b".to_string();
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

                // let ex = self.evaluate(expr, &Vec::new(), false, 0)?;
                let ex = expr;
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
                let o = self.force_evaluate(expr.clone())?.ok_or_else(|| format!("Unable to find exact set value of string.\nexpr: {:?}", expr.clone()))?;
                let s = o.interpret_as_string()?;
                out += &format!("\nDISP: {}\n", s);
            },
        }

        print!("{out}");
        self.stdout += &out;

        self.position += 1;

        Ok(())
    }

    fn evaluate(&self, expr: Expr, local: &Vec<String>, forced_evaluation: bool, depth: usize) -> Result<Expr, String> {
        println!("{}e: {:?}", " ".repeat(depth), expr);

        match expr.clone() {
            Expr::None => {Ok(expr)},

            Expr::Known(_) => {Ok(expr)},

            Expr::Identifier(name) => {
                // If the variable is local, don't evaluate
                // If it has a value, evaluate it
                // Otherwise, it is an undefined variable.

                if local.contains(&name) {
                    return Ok(expr);
                }

                match self.values.get(&name).cloned() {
                    Some(value) => {self.evaluate(value, local, forced_evaluation, depth + 1)},
                    None => {
                        if forced_evaluation {
                            Err(format!("Variable ({name}) used before it is defined."))
                        } else {
                            Ok(expr)
                        }
                    },
                }
            },

            Expr::SetLiteral(exprs) => {
                // Simplify element-wise then attempt conversion to a Known

                let simp_exprs = exprs
                    .clone()
                    .into_iter()
                    .map(|i| self.evaluate(i, local, forced_evaluation, depth + 1))
                    .collect::<Result<Vec<Expr>, String>>()?;

                let simp = Expr::SetLiteral(simp_exprs);

                if let Ok(set) = simp.to_set() {
                    Ok(Expr::Known(set))
                } else {
                    Ok(simp)
                }
            },

            Expr::ForAll(expr, name) => {

                // If it is within a local expression, there is some hope of it being able to be evaluated later
                // Otherwise, proceed with backup evaluation by looking at membership checks and attempting to infer elements

                if !forced_evaluation {
                    // Leave partly unevaluated, simplifying the subexpression
                    // Also adds on a new local variable

                    // Expressions evaluated depth-first, so check if name is used elsewhere and if so change it
                    // e.g. if you have {{a for all a} for all a}, then it becomes {{a$1 for all a$1} for all a$0}
                    let mut counter: u32 = 0;
                    let new_base_name = name.split('$').next().unwrap();
                    while local.contains(&format!("{new_base_name}${counter}")) {counter += 1;}
                    while self.values.contains_key(&format!("{new_base_name}${counter}")) {counter += 1;}
                    let name_collision_replacement = format!("{new_base_name}${counter}");
                    
                    let mut new_local = local.clone();
                    new_local.push(name_collision_replacement.clone());
                    return Ok(Expr::ForAll(
                        Box::new(self.evaluate(
                            expr.substitute(name, Expr::Identifier(name_collision_replacement.clone()))?,
                            &new_local,
                            forced_evaluation,
                            depth + 1
                        )?),
                        name_collision_replacement
                    ))
                }

                println!("USING BACKUP FORALL EVALUATION SYSTEM.");

                let members = expr.contains_membership_checks_of();
                println!("Members: {:?}", members);
                if members.len() == 0 {
                    return Err(format!("Backup ForAll evaluation failed: No membership checks.\nExpr: {:?}", *expr));
                }

                let mut all_elements = Vec::new();

                for member in members {
                    all_elements.extend(self.force_evaluate(member)?.ok_or(format!("Ruh roh"))?.s.into_iter());
                }

                let mut o = Vec::new();

                for e in all_elements {
                    o.push(expr.clone().substitute(name.clone(), Expr::Known(e))?);
                }

                self.evaluate(Expr::SetLiteral(o), local, forced_evaluation, depth + 1)
            },

            Expr::Application(func, arg) => {
                // Depending on the function type, either attempt evaluation or defer until more is known

                match *func.clone() {
                    Expr::Known(set) => {
                        let mut log = "".to_string();

                        for s in set.s.clone() {
                            log += &format!("attempting to destructure {:?}", s);

                            if let Ok((inp, out)) = Expr::Known(s).destructure_tuple() {
                                log += &format!("({inp:?}, {out:?})");

                                if inp == *arg {
                                    return Ok(out);
                                }
                            }
                        }

                        if set.s.len() == 0 {
                            log += "empty set"
                        }

                        if forced_evaluation {
                            return Err(format!("No Matching Input Found in KnownSet Function Application.\nFunc: {:?}\nArg: {:?}\nLog: {}", func, expr, log))
                        } else {
                            return Ok(expr);
                        }
                    },

                    Expr::SetLiteral(set) => {
                        let mut log = "".to_string();

                        for s in set.clone() {
                            log += &format!("attempting to destructure {:?}", s);

                            if let Ok((inp, out)) = s.destructure_tuple() {
                                log += &format!("({inp:?}, {out:?})");

                                if inp == *arg {
                                    return Ok(out);
                                }
                            }
                        }

                        if set.len() == 0 {
                            log += "empty set"
                        }

                        if forced_evaluation {
                            return Err(format!("No Matching Input Found in SetLiteral Function Application.\nFunc: {:?}\nArg: {:?}\nLog: {}", func, expr, log))
                        } else {
                            return Ok(expr);
                        }
                    },
                    
                    Expr::ForAll(expr, name) => {
                        // Search for single-variable replacement {(a, f(a)) for all a}

                        match expr.clone().destructure_tuple() {
                            Ok((t1, t2)) => {

                                // If of the form {(a, f(a)) for all a}@x
                                if t1 == Expr::Identifier(name.clone()) {
                                    // replace with f(x)
                                    let substituted = t2.substitute(name, *arg)?;
                                    println!("Substituted: {:?}", substituted);
                                    self.evaluate(substituted, local, forced_evaluation, depth + 1)
                                } else {
                                    Err(format!("Pattern matching not supported at this time."))
                                }
                            },

                            Err(e) => {
                                Err(format!("Unable to interpret ForAll expression as a tuple map.\nExpr: {:?}\nErr: {}", *expr, e))
                            }
                        }
                    },

                    _ => {
                        // If not of a directly computable function type, try simplifying and try again

                        let simp_func = self.evaluate(*func.clone(), local, false, depth + 1)?;
                        println!("Original from which the above was the function: {expr:?}");
                        let simp_arg  = self.evaluate(*arg.clone(),  local, false, depth + 1)?;
                        println!("Original: {expr:?}");
                        println!("Function simplified to: {simp_func:?}");
                        println!("Argument simplified to: {simp_arg:?}");

                        let unchanged = simp_func == *func && simp_arg == *arg;

                        let application = Expr::Application(Box::new(simp_func), Box::new(simp_arg));

                        if unchanged {
                            Ok(application)
                        } else {
                            self.evaluate(application, local, false, depth + 1)

                        }
                    }
                }
            },

            Expr::Membership(elem, set) => {
                match self.evaluate(*set.clone(), local, forced_evaluation, depth)? {
                    Expr::Known(s) => {
                        if let Expr::Known(e) = *elem {
                            if s.s.contains(&e) {
                                return Ok(Expr::one());
                            }
                        }

                        Ok(Expr::zero())
                    },

                    Expr::SetLiteral(exprs) => {
                        let eelem = self.evaluate(*elem, local, false, depth + 1)?;

                        Ok(Expr::from_bool(
                            exprs.into_iter()
                            .any(|expr| self.evaluate(expr, local, false, depth + 1) == Ok(eelem.clone()))
                        ))
                    },

                    Expr::None => {
                        Err(format!("Acheivement Unlocked: How did we get here?\nSeriously though, I told you not to use 'none'."))
                    },

                    Expr::Application(func, arg) => {
                        Err(format!("Attempted membership check of an unevaluatable function application."))
                    },

                    Expr::Identifier(_) => {
                        if forced_evaluation {
                            Err(format!("I think this is unreachable. Membership check on unevaluatable identifier."))
                        } else {
                            Ok(expr)
                        }
                    },

                    Expr::Membership(_, _) => {
                        Err(format!("I think this is unreachable. Membership check on unevaluatable membership. Also, what the heck are you trying to do???"))
                    },

                    Expr::ForAll(expr, name) => {
                        todo!()
                    }
                }
            },
        }
    }

    fn force_evaluate(&self, original: Expr) -> Result<Option<Set>, String> {
        let mut expr = original;
        let mut prev = Expr::None;
        while !prev.eq(&expr) {
            println!("P,C: {:?} | {:?}", prev, expr);
            prev = expr.clone();
            expr = self.evaluate(expr, &Vec::new(), true, 0)?;
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

        let literal_self = self.to_literal();

        println!("Destructuring: {:?}", literal_self);

        if let Expr::SetLiteral(exprs) = literal_self {
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
                } else if let (Expr::Known(e1s), Expr::SetLiteral(e2)) = (exprs[0].clone(), exprs[1].clone()) {
                    let e1 = match e1s.to_literal() {Expr::SetLiteral(o) => o, _ => unreachable!()};
                    
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
