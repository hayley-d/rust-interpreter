pub mod abstract_syntax_tree {
    use crate::{Expression, Token};

    pub struct Node<'a> {
        children: Vec<Expression<'a>>,
        terminal: bool,
        token: Token,
    }

    pub struct AbstractSyntaxTree<'a> {
        root: Node<'a>,
    }

    impl<'a> AbstractSyntaxTree<'a> {
        pub fn new() -> Self {
            todo!()
        }
    }

    impl<'a> Node<'a> {
        pub fn new(token: Token) -> Self {
            return Node {
                children: Vec::with_capacity(1),
                terminal: true,
                token,
            };
        }
    }
}
