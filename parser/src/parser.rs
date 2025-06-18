use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;

use ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression, FunctionLiteral, Identifier,
    IfExpression, IndexExpression, InfixExpression, IntegerLiteral, PrefixExpression, Program,
    Statement, StringLiteral,
};
use lexer::{Lexer, Token, TokenType};

type PrefixParseFn<'a> = for<'b> fn(&'b mut Parser<'a>) -> Expression;
type InfixParseFn<'a> = for<'b> fn(&'b mut Parser<'a>, Expression) -> Expression;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PRECEDENCE {
    INT,
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > OR
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -x OR !x
    CALL,        // MYfUNCTION(x)
    INDEX,       // array[index]
}

impl PRECEDENCE {
    pub fn numeric(self) -> u8 {
        self as u8
    }
}

lazy_static! {
    pub static ref PRECEDENCES: HashMap<TokenType, PRECEDENCE> = {
        [
            (TokenType::EQ, PRECEDENCE::EQUALS),
            (TokenType::UNEQ, PRECEDENCE::EQUALS),
            (TokenType::LT, PRECEDENCE::LESSGREATER),
            (TokenType::GT, PRECEDENCE::LESSGREATER),
            (TokenType::PLUS, PRECEDENCE::SUM),
            (TokenType::MINUS, PRECEDENCE::SUM),
            (TokenType::SLASH, PRECEDENCE::PRODUCT),
            (TokenType::ASTERISK, PRECEDENCE::PRODUCT),
            (TokenType::LPAREN, PRECEDENCE::CALL),
            (TokenType::LBRACKET, PRECEDENCE::INDEX),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr: Token,
    peek: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn<'a>>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();

        let mut parser = Parser {
            lexer,
            curr,
            peek,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        parser.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::IF, Parser::parse_if_expression);
        parser.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);
        parser.register_prefix(TokenType::STRING, Parser::parse_string);
        parser.register_prefix(TokenType::LBRACKET, Parser::parse_array);
        parser.register_prefix(TokenType::LBRACE, Parser::parse_hash);

        parser.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::UNEQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LPAREN, Parser::parse_call_expression);
        parser.register_infix(TokenType::LBRACKET, Parser::parse_index_expression);

        parser
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression::Boolean(Boolean {
            token: self.curr.clone(),
            value: self.curr_token_is(&TokenType::TRUE),
        })
    }

    fn parse_integer_literal(&mut self) -> Expression {
        Expression::IntegerLiteral(IntegerLiteral {
            token: self.curr.clone(),
            value: self.curr.literal.parse::<i64>().unwrap(),
        })
    }

    fn parse_string(&mut self) -> Expression {
        Expression::String(StringLiteral {
            token: self.curr.clone(),
            value: self.curr.literal.to_string(),
        })
    }

    fn parse_hash(&mut self) -> Expression {
        let token = self.curr.clone();
        let mut pairs = vec![];
        while !self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(PRECEDENCE::LOWEST);
            if !self.expect_peek_token(TokenType::COLON) {
                panic!("Expected `:`, got={:?}", self.peek)
            }
            self.next_token();
            let value = self.parse_expression(PRECEDENCE::LOWEST);
            pairs.push((key, value));
            if !self.peek_token_is(&TokenType::RBRACE) && !self.expect_peek_token(TokenType::COMMA)
            {
                panic!("invalid syntax")
            }
        }
        self.expect_peek_token(TokenType::RBRACE);
        Expression::HashMap(ast::HashLiteral { token, pairs })
    }

    fn parse_array(&mut self) -> Expression {
        Expression::Array(ArrayLiteral {
            token: self.curr.clone(),
            elements: self.parse_expression_list(TokenType::RBRACKET),
        })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Expression> {
        let mut list: Vec<Expression> = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expression(PRECEDENCE::LOWEST));
        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(PRECEDENCE::LOWEST));
        }
        self.expect_peek_token(end);
        list
    }

    fn parse_index_expression(&mut self, left: Expression) -> Expression {
        let token = self.curr.clone();
        self.next_token();
        let index = self.parse_expression(PRECEDENCE::LOWEST);
        self.expect_peek_token(TokenType::RBRACKET);
        Expression::Index(IndexExpression {
            token,
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn peek_precedence(&self) -> PRECEDENCE {
        if PRECEDENCES.contains_key(&self.peek.kind) {
            return PRECEDENCES[&self.peek.kind];
        }
        PRECEDENCE::LOWEST
    }

    fn curr_precedence(&self) -> PRECEDENCE {
        if PRECEDENCES.contains_key(&self.curr.kind) {
            return PRECEDENCES[&self.curr.kind];
        }
        PRECEDENCE::LOWEST
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let block_token = self.curr.clone();
        let mut statements = vec![];
        self.next_token();

        while !self.curr_token_is(&TokenType::RBRACE) && !self.curr_token_is(&TokenType::EOF) {
            let statement = self.parse_statement().unwrap();
            statements.push(statement);
            self.next_token();
        }
        BlockStatement {
            token: block_token,
            statements,
        }
    }

    fn parse_if_expression(&mut self) -> Expression {
        let current_token = self.curr.clone();
        self.expect_peek_token(TokenType::LPAREN);
        self.next_token();
        let condition = self.parse_expression(PRECEDENCE::LOWEST);
        self.expect_peek_token(TokenType::RPAREN);
        self.expect_peek_token(TokenType::LBRACE);

        let consequence = self.parse_block_statement();
        let alternative = if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();
            self.expect_peek_token(TokenType::LBRACE);
            Some(self.parse_block_statement())
        } else {
            None
        };

        Expression::IfExpression(IfExpression {
            token: current_token,
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> Expression {
        let token = self.curr.clone();
        self.expect_peek_token(TokenType::LPAREN);
        let parameters = self.parse_function_parameters();
        self.expect_peek_token(TokenType::LBRACE);

        let body = self.parse_block_statement();
        Expression::FunctionLiteral(FunctionLiteral {
            token,
            parameters: Rc::new(parameters),
            body: Rc::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];
        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }
        self.next_token();
        identifiers.push(Identifier {
            token: self.curr.clone(),
            value: self.curr.literal.clone(),
        });

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier {
                token: self.curr.clone(),
                value: self.curr.literal.clone(),
            });
        }

        if !self.expect_peek_token(TokenType::RPAREN) {
            panic!("Expected closing parenthesis, found {}", self.peek.literal);
        };

        identifiers
    }

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        Expression::CallExpression(CallExpression {
            token: self.curr.clone(),
            function: Box::new(function),
            arguments: self.parse_expression_list(TokenType::RPAREN),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.curr.clone();
        let operator = self.curr.literal.to_string();
        let right_precedence = self.curr_precedence();
        self.next_token();
        let right = self.parse_expression(right_precedence);
        Expression::InfixExpression(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.curr.clone();
        let operator = self.curr.literal.to_string();
        self.next_token();
        let right = self.parse_expression(PRECEDENCE::PREFIX);
        Expression::PrefixExpression(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(ast::Identifier {
            token: self.curr.clone(),
            value: self.curr.literal.clone(),
        })
    }

    fn register_prefix(&mut self, token: TokenType, prefix_parse_fn: PrefixParseFn<'a>) {
        self.prefix_parse_fns.insert(token, prefix_parse_fn);
    }

    fn register_infix(&mut self, token: TokenType, infix_parse_fn: InfixParseFn<'a>) {
        self.infix_parse_fns.insert(token, infix_parse_fn);
    }

    fn next_token(&mut self) {
        self.curr = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn peek_token_is(&mut self, t: &TokenType) -> bool {
        self.peek.kind == *t
    }

    fn curr_token_is(&self, t: &TokenType) -> bool {
        self.curr.kind == *t
    }

    fn expect_peek_token(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            true
        } else {
            self.peek_error(&token);
            false
        }
    }

    fn peek_error(&mut self, expected_token: &TokenType) {
        let message = format!(
            "expected next token to be {:?}, got {:?}",
            *expected_token, self.peek.kind
        );
        self.errors.push(message);
    }

    pub fn print_errors(self) {
        if self.errors.is_empty() {
            println!("No errors")
        } else {
            for err in self.errors {
                println!("Error: {}", err);
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.curr.clone();
        self.next_token();
        let value = self.parse_expression(PRECEDENCE::LOWEST);
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::Return { token, value })
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.curr.clone();

        if !self.expect_peek_token(TokenType::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.curr.clone(),
            value: self.curr.literal.clone(),
        };

        if !self.expect_peek_token(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(PRECEDENCE::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Let { token, name, value })
    }

    fn parse_expression(&mut self, precedence: PRECEDENCE) -> Expression {
        let mut prefix;
        if let Some(prefix_parse_fn) = self.prefix_parse_fns.get(&self.curr.kind) {
            prefix = prefix_parse_fn(self);
        } else {
            panic!("Cannot parse prefix: {}", self.curr.literal)
        }

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            if let Some(infix_parse_fn) = self.infix_parse_fns.get(&self.peek.kind).cloned() {
                self.next_token();
                prefix = infix_parse_fn(self, prefix);
            } else {
                return prefix;
            };
        }
        prefix
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.kind {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();
        let expr = self.parse_expression(PRECEDENCE::LOWEST);
        if !self.expect_peek_token(TokenType::RPAREN) {
            panic!("Unclosed Parenthesis")
        }
        expr
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = Statement::Expression {
            token: self.curr.clone(),
            value: self.parse_expression(PRECEDENCE::LOWEST),
        };

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(expr)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        while self.curr.kind != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }
        Program { statements }
    }
}
