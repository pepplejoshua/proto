use proto::ast::{Expression, ProtoBinaryOp, ProtoNode, ProtoStruct, Statement};
use proto::{common::ProtoErr, lexer::TokenKind, parser::parse};
use std::{fs, rc::Rc};

mod common;
use crate::common::{
    is_boolean_literal, is_char_literal, is_numeric_literal, is_operator, is_string_literal,
};

#[test]
fn tsest_parsing_literals() -> Result<(), ProtoErr> {
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
fn test_parser_binop_simple() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/binary_operation.pr";
    let src = fs::read_to_string(path).unwrap();

    let res = parse(src)?;
    let contents = res.program;
    assert_eq!(contents.len(), 1);
    let bin_op = contents[0].clone();

    // peel back the nesting
    if let ProtoNode::ProtoExpr(Expression::BinaryOp(ProtoBinaryOp {
        left,
        right,
        operator,
    })) = bin_op
    {
        is_numeric_literal(left, 1)?;
        is_numeric_literal(right, 2)?;
        is_operator(
            operator, TokenKind::Plus)?;
    } else {
        // For some reason binop returns a different AST
        return Err(ProtoErr::General(
            "Unknown AST node returned from expression".into(),
            None,
        ));
    }
    Ok(())
}

#[test]
fn test_parser_struct_id_and_members() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/parser/valid/struct_id_and_members.pr";
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
