package ast

import (
	"proto/lexer"
	"strings"
)

type Expression interface { // anything that doesn't implement this is a statement
	ProtoNode
	Type() ProtoType
}

type Identifier struct {
	Token   lexer.ProtoToken
	Id_Type ProtoType
}

func (i *Identifier) LiteralRepr() string {
	return i.Token.Literal
}

func (i *Identifier) Type() ProtoType {
	return i.Id_Type
}

type BinaryOp struct {
	Left     Expression
	Right    Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (b *BinaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(b.Operator.Literal)
	msg.WriteString(" " + b.Left.LiteralRepr())
	msg.WriteString(" " + b.Right.LiteralRepr() + ")")

	return msg.String()
}

func (b *BinaryOp) Type() ProtoType {
	return b.Op_Type
}

type UnaryOp struct {
	Operand  Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (u *UnaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(u.Operator.Literal)
	msg.WriteString(" " + u.Operand.LiteralRepr() + ")")

	return msg.String()
}

func (u *UnaryOp) Type() ProtoType {
	return u.Op_Type
}

type I64 struct {
	Token lexer.ProtoToken
}

func (i *I64) LiteralRepr() string {
	return i.Token.Literal
}

func (i *I64) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.I64_TYPE,
			Literal:   "i64",
			TokenSpan: i.Token.TokenSpan,
		},
	}
}

type Char struct {
	Token lexer.ProtoToken
}

func (c *Char) LiteralRepr() string {
	return "'" + c.Token.Literal + "'"
}

func (c *Char) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.CHAR_TYPE,
			Literal:   "char",
			TokenSpan: c.Token.TokenSpan,
		},
	}
}

type String struct {
	Token lexer.ProtoToken
}

func (s *String) LiteralRepr() string {
	return s.Token.Literal
}

func (s *String) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.STRING_TYPE,
			Literal:   "str",
			TokenSpan: s.Token.TokenSpan,
		},
	}
}

type Boolean struct {
	Value bool
	Token lexer.ProtoToken
}

func (b *Boolean) LiteralRepr() string {
	return b.Token.Literal
}

func (b *Boolean) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.BOOL_TYPE,
			Literal:   "bool",
			TokenSpan: b.Token.TokenSpan,
		},
	}
}

type Array struct {
	Items     []Expression
	Token     lexer.ProtoToken
	ArrayType *Proto_Array
}

func (a *Array) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("[")

	for index, item := range a.Items {
		str.WriteString(item.LiteralRepr())

		if index+1 < len(a.Items) {
			str.WriteString(", ")
		}
	}
	str.WriteString("]")
	return str.String()
}

func (a *Array) Type() ProtoType {
	return a.ArrayType
}

type Tuple struct {
	Items     []Expression
	Token     lexer.ProtoToken
	TupleType *Proto_Tuple
}

func (t *Tuple) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("(")

	for index, item := range t.Items {
		str.WriteString(item.LiteralRepr())

		if index+1 < len(t.Items) {
			str.WriteString(", ")
		}
	}
	str.WriteString(")")
	return str.String()
}

func (t *Tuple) Type() ProtoType {
	return t.TupleType
}
