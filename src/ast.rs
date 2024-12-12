pub mod abstract_syntax_tree {
    use crate::Token;

    pub struct TerminalNode {
        value: Token,
    }

    pub struct NonTerminalNode {
        children: Vec<Token>,
    }
}
