#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub identifier: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipe {
    pub source: Expression,
    pub sink: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub pipe: Pipe,
}
