use super::{eval_file, unify, vars::Vars, TRACE};
use crate::{
    atom,
    database::Database,
    errors::Error,
    solver::math,
    types::{
        ConsIter,
        Term::{self, *},
    },
};
use std::slice;

/// If the question was satisfied (exit) or not (fail).
pub type Status = bool;

// It would be nicer to have this as a trait, but the compiler does not like
// having dynamic fields in structs and complains about lifetimes.
// The suggestion to make it an enum comes from the compiler itself.
#[derive(Clone, Debug)]
#[allow(private_interfaces)]
pub enum ByrdBox {
    /// Does classical search and unification for the goal.
    Unify(Unify),
    /// Evaluates a function with special evaluation rules.
    Eval(Eval),
    /// Negates the result of the contained goal, then it changes
    /// the boolean flag forcing every coming call to fail.
    Not(Box<ByrdBox>, bool),
    /// Series of goals to satisfy.
    And(And),
    /// Has two alternating clauses.
    Or(Or),
    /// When the argument is `None` that's the cut (`!`) operator.
    /// Otherwise, it marks `Some(goal)`, which preceded `!`, not to be backtracked,
    /// ```
    /// // ?-     goal1,            goal2,       !,     goal4, ...
    /// [Cut(Some(goal1)), Cut(Some(goal2)), Cut(None), goal4, ...]
    /// ```
    Cut(Option<Box<ByrdBox>>),
    /// A standalone variable.
    Var(Var),
}

impl ByrdBox {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        if unsafe { TRACE } {
            let goal = self.materialize(vars);
            println!("CALL: {}", goal);
        }

        use ByrdBox::*;
        let mut call = || match self {
            Unify(this) => this.call(vars),
            Eval(this) => this.call(vars),
            Not(_, true) => Ok(false),
            Not(this, done) => {
                *done = true;
                Ok(!this.call(vars)?)
            }
            And(this) => this.call(vars),
            Or(this) => this.call(vars),
            Cut(None) => Ok(true),
            Cut(Some(this)) => this.call(vars),
            Var(this) => this.call(vars),
        };

        let status = call()?;
        if unsafe { TRACE } {
            let goal = self.materialize(vars);
            match status {
                true => println!("EXIT: {}", goal),
                false => println!("FAIL: {}", goal),
            }
        }
        Ok(status)
    }

    /// Reset the branch of the search tree to start fresh search.
    fn reset(&mut self) {
        use ByrdBox::*;
        match self {
            Unify(this) => this.reset(),
            Not(this, done) => {
                *done = false;
                this.reset()
            }
            And(this) => this.reset(),
            Or(this) => this.reset(),
            Var(this) => this.reset(),
            _ => (),
        }
    }

    fn init(&mut self, vars: &Vars) {
        use ByrdBox::*;
        match self {
            Unify(this) => this.goal = vars.init(&this.goal),
            Eval(this) => this.args = vars.init_all(&this.args),
            Not(this, _) => this.init(vars),
            And(this) => this.init(vars),
            Or(this) => {
                this.branches[0].init(vars);
                this.branches[1].init(vars);
            }
            Var(this) => this.init(vars),
            _ => (),
        }
    }

    /// Move to the next clause in the search tree. Return status if the switch was possible.
    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        use ByrdBox::*;
        match self {
            Unify(this) => this.next_clause(vars),
            Not(_, true) => Ok(false),
            Not(this, _) => this.next_clause(vars),
            And(this) => this.next_clause(vars),
            Or(this) => this.next_clause(vars),
            Var(this) => this.next_clause(vars),
            _ => Ok(false),
        }
    }

    /// Mark this goal as `Cut` as a result of the cut (`!`) operation.
    /// Such goal cannot be backtracked.
    fn cut(&mut self) {
        use ByrdBox::Cut;
        if !matches!(self, Cut(_)) {
            *self = Cut(Some(Box::new(self.clone())));
        }
    }

    fn is_cut(&self) -> bool {
        use ByrdBox::*;
        match self {
            And(this) => this.is_cut(),
            Or(this) => this.is_cut(),
            Var(this) => this.is_cut(),
            Cut(_) => true,
            _ => false,
        }
    }

    /// Convert current state of the branch to the `Term` with variables replaced by their values.
    fn materialize(&self, vars: &Vars) -> Term {
        use ByrdBox::*;
        use Term::*;
        match self {
            Unify(this) => vars.subst(&this.goal),
            Eval(this) => {
                if this.args.is_empty() {
                    atom!(this.id)
                } else {
                    Struct(this.id.clone(), vars.subst_all(&this.args))
                }
            }
            Not(this, _) => Struct("\\+".to_string(), vec![this.materialize(vars)]),
            And(this) => this.materialize(vars),
            Or(this) => Struct(
                ";".to_string(),
                vec![
                    this.branches[0].materialize(vars),
                    this.branches[1].materialize(vars),
                ],
            ),
            Cut(None) => atom!("!"),
            Cut(Some(this)) => this.materialize(vars),
            Var(this) => this.materialize(vars),
        }
    }

    fn new(goal: &Term, db: Database) -> Result<Self, Error> {
        use Term::*;
        let this = match goal {
            // special atoms
            Atom(id) if id == "!" => ByrdBox::Cut(None),
            Atom(id) if id == "nl" || id == "fail" || id == "trace" || id == "notrace" => {
                ByrdBox::Eval(Eval::new(id.to_string(), Vec::new(), db))
            }
            // unary functions
            Struct(id, args) if args.len() == 1 && id == "\\+" => {
                let body = ByrdBox::new(&args[0], db)?;
                ByrdBox::Not(Box::new(body), false)
            }
            Struct(id, args)
                if args.len() == 1
                    && (id == "write"
                        || id == "consult"
                        || id == "integer"
                        || id == "number"
                        || id == "var"
                        || id == "atom") =>
            {
                ByrdBox::Eval(Eval::new(id.to_string(), args.clone(), db))
            }
            // binary functions
            Struct(id, args)
                // other inequalities are implemented in the standard library
                if args.len() == 2
                    && (id == "="
                        || id == "is"
                        || id == "=:="
                        || id == "<"
                        || id == "=="
                        || id == "@<") =>
            {
                ByrdBox::Eval(Eval::new(id.to_string(), args.clone(), db))
            }
            Struct(id, ref args) if args.len() == 2 && id == ";" => ByrdBox::Or(Or::new(args, db)?),
            Struct(id, ref args) if args.len() == 2 && id == "->" => {
                // `If -> Else` is a syntactic sugar for `If, !, Else`
                // it's implemented like this to be consistent with the ISO Prolog standard.
                // For `->(If, Then) :- If, !, Then.` the cut does not propagate properly,
                // and `If -> Then ; Else` does not pass the ISO Prolog tests by running also the Else branch.
                let cond = ByrdBox::new(&args[0], db.clone())?;
                let then = ByrdBox::new(&args[1], db.clone())?;
                ByrdBox::And(And::new(vec![cond, ByrdBox::Cut(None), then]))
            },
            // functions with other number of arguments
            Struct(id, args) if args.len() == 3 && id == "functor" => {
                ByrdBox::Eval(Eval::new(id.to_string(), args.clone(), db))
            }
            Struct(id, args) if id == "," => Self::from(args, db)?,
            // standalone variable
            Variable(_, _) => ByrdBox::Var(Var::new(goal.clone(), db)),
            // regular unifications
            Atom(_) | Struct(_, _) => ByrdBox::Unify(Unify::new(goal.clone(), db)?),
            _ => return Err(Error::NotCallable(goal.clone())),
        };
        Ok(this)
    }

    pub fn from(terms: &[Term], db: Database) -> Result<Self, Error> {
        let goals = boxes_from(terms, db)?;
        if goals.len() == 1 {
            Ok(goals[0].clone())
        } else {
            Ok(Self::And(And::new(goals)))
        }
    }

    pub fn iter(&self) -> Solver {
        Solver {
            this: self.clone(),
            done: false,
        }
    }
}

fn boxes_from(terms: &[Term], db: Database) -> Result<Vec<ByrdBox>, Error> {
    let mut boxes = Vec::new();
    for term in terms {
        match term {
            Term::Struct(id, args) if id == "," => {
                // flatten the ,()
                for arg in args {
                    boxes.push(ByrdBox::new(arg, db.clone())?)
                }
            }
            _ => boxes.push(ByrdBox::new(term, db.clone())?),
        }
    }
    Ok(boxes)
}

pub struct Solver {
    this: ByrdBox,
    /// The final branch of the search tree was reached.
    /// This prevents an infinite loop.
    done: bool,
}

impl Iterator for Solver {
    type Item = Result<Vars, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        // variables
        let mut vars = Vars::default();
        vars.branch();
        self.this.init(&vars);

        // search for solution
        let result = self.this.call(&mut vars);

        if unsafe { TRACE } {
            // so that there's a visual break after we end
            println!();
        }

        // move the pointer, so on the next iteration we check the next branch
        match self.this.next_clause(&vars) {
            // we cannot switch to next clause
            // to avoid an infinite loop at the last clause mark it as done
            Ok(false) => self.done = true,
            Ok(true) => (),
            Err(err) => return Some(Err(err)),
        }

        match result {
            Ok(false) => None,
            Ok(true) => {
                // post-process
                vars.prune();
                Some(Ok(vars))
            }
            Err(msg) => Some(Err(msg)),
        }
    }
}

/// Perform standard unification of the `goal` against the clauses from the `queue`.
/// The `clause` is the currently explored clause.
#[derive(Clone, Debug)]
struct Unify {
    /// The goal that is matched.
    goal: Term,
    /// The current clause it is unified against.
    clause: Option<Clause>,
    /// The queue of all the clauses to be unified against.
    queue: Vec<Term>,
    /// The position of the current clause that we unify against.
    pos: usize,
    db: Database,
}

impl Unify {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        vars.branch();
        let prev_vars = vars.len();
        loop {
            if let Some(clause) = self.clause.as_mut() {
                if clause.call(vars)? {
                    return Ok(true);
                }
            }

            // reset vars (it's append only, so we can truncate it to the previous state)
            vars.truncate(prev_vars);

            if !self.queue_next(vars)? {
                // there's no more clauses, search falseed
                // mark it as None so we don't call it again in an infinite loop
                self.clause = None;
                return Ok(false);
            }
        }
    }

    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        if let Some(clause) = self.clause.as_mut() {
            if clause.next_clause(vars)? {
                return Ok(true);
            }
        }
        self.queue_next(vars)
    }

    fn queue_next(&mut self, vars: &Vars) -> Result<bool, Error> {
        if self.is_cut() || self.is_done() {
            return Ok(false);
        }
        let goal = self.goal.clone();
        let clause = vars.init(&self.queue[self.pos]);
        self.clause = Some(Clause::new(goal, clause, self.db.clone())?);
        self.pos += 1;
        Ok(true)
    }

    /// We explored all the branches in the queue.
    fn is_done(&self) -> bool {
        self.pos >= self.queue.len()
    }

    fn reset(&mut self) {
        if !self.is_cut() {
            self.clause = None;
            self.pos = 0;
        }
    }

    /// The current clause reached the cutpoint, this means
    /// that the the cut (`!`) is applicable also to searching
    /// for further solutions.
    fn is_cut(&self) -> bool {
        if let Some(ref clause) = self.clause {
            return clause.is_cut();
        }
        false
    }

    fn new(goal: Term, db: Database) -> Result<Self, Error> {
        let queue = db.query(&goal).unwrap_or_default();
        if queue.is_empty() {
            return Err(Error::Unknown(goal.clone()));
        }
        Ok(Self {
            goal,
            clause: None,
            queue,
            pos: 0,
            db,
        })
    }
}

/// The `goal` is unified with `head`, then the `body` is evaluated.
#[derive(Clone, Debug)]
struct Clause {
    goal: Term,
    head: Term,
    body: Option<Box<ByrdBox>>,
}

impl Clause {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        if !unify(&self.goal, &self.head, vars) {
            self.body = None;
            return Ok(false);
        }

        if let Some(body) = self.body.as_mut() {
            vars.branch();
            let prev_vars = vars.len();
            if body.call(vars)? {
                return Ok(true);
            }
            // reset vars (it's append only, so we can truncate it to the previous state)
            vars.truncate(prev_vars);
            return Ok(false);
        }
        Ok(true)
    }

    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        if let Some(body) = self.body.as_mut() {
            return body.next_clause(vars);
        }
        Ok(false)
    }

    fn is_cut(&self) -> bool {
        if let Some(ref body) = self.body {
            return body.is_cut();
        }
        false
    }

    fn new(goal: Term, clause: Term, db: Database) -> Result<Self, Error> {
        let (head, body) = match clause {
            Rule(head, ref body) => {
                let body = Box::new(ByrdBox::from(body, db)?);
                (*head.clone(), Some(body))
            }
            _ => (clause, None),
        };
        Ok(Self { goal, head, body })
    }
}

/// Call an expression with special evaluation rules (e.g. arithmetic operations).
/// The `id` is the name of the evaluated function, `args` are it's arguments (can be empty),
/// and `done` is the status flag signaling that it was already evaluated, so there is nothing
/// more to explore here (to avoid infinite loops when calling it again).
#[derive(Clone, Debug)]
struct Eval {
    id: String,
    args: Vec<Term>,
    db: Database,
}

impl Eval {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        let args = vars.subst_all(&self.args);

        match self.id.as_str() {
            "fail" => Ok(false),
            "=" => Ok(unify(&args[0], &args[1], vars)),
            "is" => {
                let (lhs, rhs) = (&args[0], &args[1]);
                let result = math::eval(rhs.clone(), vars)?;
                match lhs {
                    Variable(_, _) => match vars.get(lhs) {
                        Some(val) => Ok(val == &result),
                        None => {
                            vars.insert(lhs.clone(), result.clone());
                            Ok(true)
                        }
                    },
                    Number(_) => Ok(lhs == &result),
                    _ => Err(Error::TypeError(lhs.clone())),
                }
            }
            "=:=" => {
                let (lhs, rhs) = math::eval_args(&args, vars)?;
                Ok(lhs == rhs)
            }
            "<" => {
                let (lhs, rhs) = math::eval_args(&args, vars)?;
                Ok(lhs < rhs)
            }
            "==" => {
                // _ \== _ according to ISO Prolog
                if args[0] == Any || args[1] == Any {
                    return Ok(false);
                }
                Ok(args[0] == args[1])
            }
            "@<" => Ok(vars.cmp(&args[0], &args[1]) == std::cmp::Ordering::Less),
            "nl" => {
                println!();
                Ok(true)
            }
            "integer" | "number" => Ok(matches!(args[0], Number(_))),
            "var" => {
                Ok(matches!(&self.args[0], Variable(_, _)) && vars.get(&self.args[0]).is_none())
            }
            "atom" => Ok(matches!(args[0], Atom(_) | Nil)),
            "write" => {
                print!("{}", args[0]);
                Ok(true)
            }
            "consult" => {
                match &args[0] {
                    Atom(path) => {
                        let path = assure_pl_extension(path);
                        eval_file(&path, self.db.clone())?;
                    }
                    list @ Struct(id, _) if id == "." => {
                        for path in ConsIter::from(list.clone()) {
                            match path {
                                Atom(ref path) => {
                                    let path = assure_pl_extension(path);
                                    eval_file(&path, self.db.clone())?;
                                }
                                other => return Err(Error::TypeError(other.clone())),
                            }
                        }
                    }
                    other => return Err(Error::TypeError(other.clone())),
                }
                Ok(true)
            }
            "trace" => {
                unsafe {
                    TRACE = true;
                }
                Ok(true)
            }
            "untrace" => {
                unsafe {
                    TRACE = false;
                }
                Ok(true)
            }
            "functor" => match &args[0] {
                Struct(name, func_args) => {
                    if !unify(&atom!(name), &args[1], vars) {
                        return Ok(false);
                    }
                    if !unify(&Number(func_args.len().try_into().unwrap()), &args[2], vars) {
                        return Ok(false);
                    }
                    Ok(true)
                }
                Atom(id) => {
                    if !unify(&atom!(id), &args[1], vars) {
                        return Ok(false);
                    }
                    if !unify(&Number(0), &args[2], vars) {
                        return Ok(false);
                    }
                    Ok(true)
                }
                _ => Ok(false),
            },
            _ => unreachable!(),
        }
    }

    fn new(id: String, args: Vec<Term>, db: Database) -> Self {
        Self { id, args, db }
    }
}

fn assure_pl_extension(path: &str) -> String {
    // like Prolog, add the extension if needed
    if std::fs::metadata(path).is_err() {
        return format!("{}.pl", path);
    }
    path.to_string()
}

#[derive(Debug, Clone)]
struct And {
    goals: Vec<ByrdBox>,
    /// We reached the final branch of the search tree, don't call it again (to avoid an infinite loop).
    done: bool,
}

impl And {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        // the checkpoints are used to garbage collect the old variables
        let mut checkpoints = vec![0; self.goals.len()];
        let mut pos = 0;

        while pos < self.goals.len() {
            checkpoints[pos] = vars.len();

            if self.goals[pos].is_cut() {
                self.cut_before(pos);
            }

            match self.goals[pos].call(vars)? {
                true => {
                    pos += 1;
                }
                false => {
                    // try other branch or backtrack
                    match self.backtrack(pos, vars)? {
                        Some(i) => {
                            pos = i;
                            vars.truncate(checkpoints[pos]);
                        }
                        None => {
                            self.done = true;
                            return Ok(false);
                        }
                    }
                }
            }
        }
        Ok(true)
    }

    /// Mark the goals preceding the `pos` index for no backtracking.
    /// The goals following `pos` can be backtracked.
    fn cut_before(&mut self, pos: usize) {
        for i in 0..pos {
            self.goals[i].cut();
        }
    }

    fn backtrack(&mut self, mut pos: usize, vars: &Vars) -> Result<Option<usize>, Error> {
        loop {
            if self.goals[pos].next_clause(vars)? {
                return Ok(Some(pos));
            }
            if pos > 0 {
                self.goals[pos].reset();
                pos -= 1;
            } else {
                return Ok(None);
            }
        }
    }

    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        if self.done || self.goals.is_empty() {
            return Ok(false);
        }
        Ok(self.backtrack(self.goals.len() - 1, vars)?.is_some())
    }

    fn materialize(&self, vars: &Vars) -> Term {
        use Term::Struct;
        let goals: Vec<_> = self
            .goals
            .iter()
            .map(|goal| goal.materialize(vars))
            .collect();
        if goals.len() > 1 {
            Struct(",".to_string(), goals)
        } else {
            goals[0].clone()
        }
    }

    fn init(&mut self, vars: &Vars) {
        self.goals.iter_mut().for_each(|goal| goal.init(vars));
    }

    fn reset(&mut self) {
        self.goals.iter_mut().for_each(|goal| goal.reset());
        self.done = false;
    }

    fn is_cut(&self) -> bool {
        // cut, when reached, propagates to earlier goals
        // so it is enough to check the first one
        self.goals
            .first()
            .map_or(false, |goal| matches!(goal, ByrdBox::Cut(_)))
    }

    fn new(goals: Vec<ByrdBox>) -> Self {
        And { goals, done: false }
    }
}

/// Switch between alternative goals until success.
#[derive(Clone, Debug)]
struct Or {
    branches: Box<[ByrdBox; 2]>,
    pos: usize,
}

impl Or {
    fn call(&mut self, vars: &mut Vars) -> Result<Status, Error> {
        let prev_vars = vars.len();
        loop {
            if self.last_mut().call(vars)? {
                return Ok(true);
            }
            // reset vars (it's append only, so we can truncate it to the previous state)
            vars.truncate(prev_vars);
            // otherwise try next branch
            if !self.next_clause(vars)? {
                return Ok(false);
            }
        }
    }

    fn last_mut(&mut self) -> &mut ByrdBox {
        &mut self.branches[self.pos]
    }

    fn last(&self) -> &ByrdBox {
        &self.branches[self.pos]
    }

    fn is_cut(&self) -> bool {
        self.last().is_cut()
    }

    /// Reset the branch of the search tree to start fresh search.
    fn reset(&mut self) {
        if !self.is_cut() {
            self.pos = 0;
            self.branches[0].reset();
            self.branches[1].reset();
        }
    }

    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        if self.last_mut().next_clause(vars)? {
            return Ok(true);
        }
        if self.is_cut() {
            return Ok(false);
        }
        if self.pos == 0 {
            self.pos += 1;
            return Ok(true);
        }
        Ok(false)
    }

    fn new(args: &[Term], db: Database) -> Result<Self, Error> {
        debug_assert!(args.len() == 2);
        let branches = Box::new([
            ByrdBox::from(slice::from_ref(&args[0]), db.clone())?,
            ByrdBox::from(slice::from_ref(&args[1]), db)?,
        ]);
        Ok(Self { branches, pos: 0 })
    }
}

#[derive(Debug, Clone)]
struct Var {
    var: Term,
    val: Option<Box<ByrdBox>>,
    db: Database,
}

impl Var {
    fn call(&mut self, vars: &mut Vars) -> Result<bool, Error> {
        if let Some(this) = self.val.as_mut() {
            return this.call(vars);
        }
        if self.init_val(vars)? {
            return self.call(vars);
        }
        let var = vars.find_origin(self.var.clone());
        Err(Error::UnsetVar(var.to_string()))
    }

    fn next_clause(&mut self, vars: &Vars) -> Result<bool, Error> {
        if let Some(val) = self.val.as_mut() {
            return val.next_clause(vars);
        }
        self.init_val(vars)
    }

    fn init_val(&mut self, vars: &Vars) -> Result<bool, Error> {
        if let Some(term) = vars.get(&self.var) {
            let goal = ByrdBox::new(term, self.db.clone())?;
            self.val = Some(Box::new(goal));
            return Ok(true);
        }
        Ok(false)
    }

    fn is_cut(&self) -> bool {
        if let Some(val) = self.val.as_ref() {
            return val.is_cut();
        }
        false
    }

    fn materialize(&self, vars: &Vars) -> Term {
        vars.subst(&self.var)
    }

    fn init(&mut self, vars: &Vars) {
        self.var = vars.init(&self.var);
    }

    fn reset(&mut self) {
        self.val = None;
    }

    fn new(var: Term, db: Database) -> Self {
        Self { var, val: None, db }
    }
}
