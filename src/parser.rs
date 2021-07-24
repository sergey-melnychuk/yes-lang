enum Expression {
    Const(String),
    Prefix(Operator, Box<Expression>),
    Infix(Box<Expression>, Operator, Box<Expression>),
}

#[derive(Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Not,
    Bind,
}

impl Operator {
    fn rank(&self) -> usize {
        match self {
            Operator::Mul | Operator::Div | Operator::Mod | Operator::Not => 4,
            Operator::Add | Operator::Sub => 3,
            Operator::Eq
            | Operator::Ne
            | Operator::Lt
            | Operator::Lte
            | Operator::Gt
            | Operator::Gte => 2,
            Operator::Bind => 1,
        }
    }
}
