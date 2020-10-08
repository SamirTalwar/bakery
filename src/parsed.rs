#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub expression: Positioned<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Command {
        command: Token,
        arguments: Vec<Token>,
    },
    Pipe {
        source: Positioned<Expression>,
        sink: Positioned<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Raw { value: String },
    Identifier { namespace: String, id: String },
    Text { contents: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Positioned<T> {
    pub position: Position,
    pub value: Box<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}
