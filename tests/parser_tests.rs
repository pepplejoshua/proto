use proto::ast::{Expression, ProtoBinaryOp, ProtoNode, ProtoStruct, ProtoUnaryOp, Statement};
use proto::{common::ProtoErr, lexer::TokenKind, parser::parse};
use std::{fs, rc::Rc};

mod common;
use crate::common::{
    is_boolean_literal, is_char_literal, is_expected_token_kind, is_numeric_literal,
    is_string_literal,
};

#[test]
fn test_parsing_literals() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/literals.pr";
    let src = fs::read_to_string(path).unwrap();

    let res = parse(src)?;
    let contents = res.program;
    assert_eq!(contents.len(), 7);

    // check the 2 integers
    let int_1 = contents[0].clone();
    let int_2 = contents[1].clone();

    if let ProtoNode::ProtoExpr(lit) = int_1 {
        is_numeric_literal(Rc::new(lit), 1)?;
    }

    if let ProtoNode::ProtoExpr(lit) = int_2 {
        is_numeric_literal(Rc::new(lit), 2)?;
    }

    // checek the 2 strings
    let string_1 = contents[2].clone();
    let string_2 = contents[3].clone();
    if let ProtoNode::ProtoExpr(lit) = string_1 {
        is_string_literal(Rc::new(lit), "\"string\"".into())?;
    }

    if let ProtoNode::ProtoExpr(lit) = string_2 {
        is_string_literal(Rc::new(lit), "\"\"".into())?;
    }

    // check the character
    let chr = contents[4].clone();
    if let ProtoNode::ProtoExpr(lit) = chr {
        is_char_literal(Rc::new(lit), 'c')?;
    }

    // check the 2 boolean values
    let true_lit = contents[5].clone();
    let false_lit = contents[6].clone();
    if let ProtoNode::ProtoExpr(lit) = true_lit {
        is_boolean_literal(Rc::new(lit), true)?;
    }

    if let ProtoNode::ProtoExpr(lit) = false_lit {
        is_boolean_literal(Rc::new(lit), false)?;
    }

    Ok(())
}

#[test]
fn test_parsing_unary_operations() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/unary_operations.pr";
    let src = fs::read_to_string(path).unwrap();

    let res = parse(src)?;
    let contents = res.program;
    assert_eq!(contents.len(), 3);
    let mut iter = contents.iter();

    // check integer unary operations
    let expected = vec![(300, TokenKind::Minus)];

    for (exp_operand, exp_operator) in expected {
        let un_op = (*iter.next().unwrap()).clone();

        if let ProtoNode::ProtoExpr(Expression::UnaryOp(ProtoUnaryOp { right, operator })) = un_op {
            is_numeric_literal(right, exp_operand)?;
            is_expected_token_kind(operator, exp_operator)?;
        } else {
            // For some reason binop returns a different AST
            return Err(ProtoErr::General(
                "Unknown AST node returned instead of Unary Expression".into(),
                None,
            ));
        }
    }
    // check boolean unary operations
    let expected = vec![(true, TokenKind::Not), (false, TokenKind::Not)];

    for (exp_operand, exp_operator) in expected {
        let un_op = (*iter.next().unwrap()).clone();

        if let ProtoNode::ProtoExpr(Expression::UnaryOp(ProtoUnaryOp { right, operator })) = un_op {
            is_boolean_literal(right, exp_operand)?;
            is_expected_token_kind(operator, exp_operator)?;
        } else {
            // For some reason binop returns a different AST
            return Err(ProtoErr::General(
                "Unknown AST node returned instead of Unary Expression".into(),
                None,
            ));
        }
    }

    Ok(())
}

#[test]
fn test_parsing_binary_operations() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/binary_operations.pr";
    let src = fs::read_to_string(path).unwrap();

    let res = parse(src)?;
    let contents = res.program;
    assert_eq!(contents.len(), 15);
    let mut iter = contents.iter();

    // check integer binary operations
    let expected = vec![
        (1, 2, TokenKind::Plus),
        (3, 1, TokenKind::Minus),
        (3, 2, TokenKind::Star),
        (3, 4, TokenKind::Slash),
        (4, 2, TokenKind::Modulo),
        (1, 2, TokenKind::LessThan),
        (3, 1, TokenKind::GreaterThan),
        (3, 3, TokenKind::GreaterThanOrEqual),
        (2, 3, TokenKind::LessThanOrEqual),
        (2, 3, TokenKind::ComparisonNotEquals),
        (1, 2, TokenKind::ComparisonEquals),
    ];

    for (exp_l, exp_r, exp_op) in expected {
        let bin_op = (*iter.next().unwrap()).clone();
        // peel back the nesting
        if let ProtoNode::ProtoExpr(Expression::BinaryOp(ProtoBinaryOp {
            left,
            right,
            operator,
        })) = bin_op
        {
            is_numeric_literal(left, exp_l)?;
            is_numeric_literal(right, exp_r)?;
            is_expected_token_kind(operator, exp_op)?;
        } else {
            // For some reason binop returns a different AST
            return Err(ProtoErr::General(
                "Unknown AST node returned instead of Binary Expression".into(),
                None,
            ));
        }
    }

    // check boolean binary operations
    let expected = vec![
        (true, false, TokenKind::Or),
        (false, true, TokenKind::And),
        (true, true, TokenKind::ComparisonEquals),
        (false, true, TokenKind::ComparisonNotEquals),
    ];

    for (exp_l, exp_r, exp_op) in expected {
        let bin_op = (*iter.next().unwrap()).clone();
        if let ProtoNode::ProtoExpr(Expression::BinaryOp(ProtoBinaryOp {
            left,
            right,
            operator,
        })) = bin_op
        {
            is_boolean_literal(left, exp_l)?;
            is_boolean_literal(right, exp_r)?;
            is_expected_token_kind(operator, exp_op)?;
        } else {
            // For some reason binop returns a different AST
            return Err(ProtoErr::General(
                "Unknown AST node returned from expression".into(),
                None,
            ));
        }
    }

    Ok(())
}

#[test]
fn test_parsing_struct() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/structs.pr";
    let src = fs::read_to_string(path).unwrap();

    let res = parse(src.clone())?;
    let contents = res.program;
    assert_eq!(contents.len(), 1);
    let proto_struct = contents[0].clone();

    if let ProtoNode::ProtoStatement(Statement::Struct(ProtoStruct {
        identifier,
        members,
    })) = proto_struct
    {
        let id_span = identifier.span;
        let identifier: &str = src[id_span.start..id_span.end].as_ref();
        assert_eq!(identifier, "Person");

        let expected_members = ["name", "age"];

        assert_eq!(expected_members.len(), members.len());

        for (span, _) in members.iter() {
            let identifier = &src[span.start..span.end];
            // Note: We must use contains, as a hashmap converting to an iter is
            // 		 not 100% reliable with the order, so we can't directly compare.
            assert!(expected_members.contains(&identifier));
        }
    } else {
        // For some reason record returns a new AST, or is updated and the test was not
        return Err(ProtoErr::General(
            "Unknown AST node returned from record".into(),
            None,
        ));
    }

    Ok(())
}
