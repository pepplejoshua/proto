use std::rc::Rc;

use proto::{
    ast::{Expression, Var},
    common::ProtoErr,
    lexer::{Token, TokenKind},
};

pub fn is_expected_token_kind(token: Rc<Token>, expected: TokenKind) -> Result<(), ProtoErr> {
    if token.kind == expected {
        Ok(())
    } else {
        Err(ProtoErr::General(
            format!(
                "Operators expected {:?} but received {:?}",
                expected, token.kind
            ),
            None,
        ))
    }
}

pub fn is_numeric_literal(ast: Rc<Expression>, expected: i64) -> Result<(), ProtoErr> {
    if let Expression::Literal(literal) = (*ast).clone() {
        if literal.kind == TokenKind::Integer(expected) {
            Ok(())
        } else {
            Err(ProtoErr::General(
                format!(
                    "Literal expected {expected} but received {:?}",
                    literal.kind
                ),
                None,
            ))
        }
    } else {
        Err(ProtoErr::General("Node is not a literal".into(), None))
    }
}

pub fn is_string_literal(ast: Rc<Expression>, expected: String) -> Result<(), ProtoErr> {
    if let Expression::Literal(literal) = (*ast).clone() {
        if literal.kind == TokenKind::String(expected.clone()) {
            Ok(())
        } else {
            Err(ProtoErr::General(
                format!(
                    "Literal expected {expected} but received {:?}",
                    literal.kind
                ),
                None,
            ))
        }
    } else {
        Err(ProtoErr::General("Node is not a literal".into(), None))
    }
}

pub fn is_char_literal(ast: Rc<Expression>, expected: char) -> Result<(), ProtoErr> {
    if let Expression::Literal(literal) = (*ast).clone() {
        if literal.kind == TokenKind::Char(expected) {
            Ok(())
        } else {
            Err(ProtoErr::General(
                format!(
                    "Literal expected {expected} but received {:?}",
                    literal.kind
                ),
                None,
            ))
        }
    } else {
        Err(ProtoErr::General("Node is not a literal".into(), None))
    }
}

pub fn is_boolean_literal(ast: Rc<Expression>, expected: bool) -> Result<(), ProtoErr> {
    if let Expression::Literal(literal) = (*ast).clone() {
        if literal.kind == TokenKind::Boolean(expected) {
            Ok(())
        } else {
            Err(ProtoErr::General(
                format!(
                    "Literal expected {expected} but received {:?}",
                    literal.kind
                ),
                None,
            ))
        }
    } else {
        Err(ProtoErr::General("Node is not a literal".into(), None))
    }
}

pub fn is_identifier(ast: Rc<Expression>, expected: String) -> Result<(), ProtoErr> {
    if let Expression::Variable(Var {
        identifier,
        var_type: _,
    }) = (*ast).clone()
    {
        if identifier.kind == TokenKind::Identifier(expected.clone()) {
            Ok(())
        } else {
            Err(ProtoErr::General(
                format!(
                    "Literal expected {expected} but received {:?}",
                    identifier.kind
                ),
                None,
            ))
        }
    } else {
        Err(ProtoErr::General("Node is not a literal".into(), None))
    }
}
