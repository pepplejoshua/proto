package ast

import (
	"proto/lexer"
	"strings"
)

type Expression interface { // anything that doesn't implement this is a statement
	ProtoNode
	Type() string
}

type Identifier struct {
	Token   lexer.ProtoToken
	Id_Type string
}

func (i *Identifier) LiteralRepr() string {
	return i.Token.Literal
}

func (i *Identifier) Type() string {
	return i.Id_Type
}

type BinaryOp struct {
	Left     Expression
	Right    Expression
	Operator lexer.ProtoToken
	Op_Type  string
}

func (b *BinaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(b.Operator.Literal)
	msg.WriteString(" " + b.Left.LiteralRepr())
	msg.WriteString(" " + b.Right.LiteralRepr() + ")")

	return msg.String()
}

func (b *BinaryOp) Type() string {
	return b.Op_Type
}

type UnaryOp struct {
	Operand  Expression
	Operator lexer.ProtoToken
	Op_Type  string
}

func (u *UnaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(u.Operator.Literal)
	msg.WriteString(" " + u.Operand.LiteralRepr() + ")")

	return msg.String()
}

func (u *UnaryOp) Type() string {
	return u.Op_Type
}

type I64 struct {
	Token lexer.ProtoToken
}

func (i *I64) LiteralRepr() string {
	return i.Token.Literal
}

func (i *I64) Type() string {
	return "i64"
}

type Char struct {
	Token lexer.ProtoToken
}

func (c *Char) LiteralRepr() string {
	return c.Token.Literal
}

func (c *Char) Type() string {
	return "char"
}

type String struct {
	Token lexer.ProtoToken
}

func (s *String) LiteralRepr() string {
	return s.Token.Literal
}

func (s *String) Type() string {
	return "str"
}

type Boolean struct {
	Value bool
	Token lexer.ProtoToken
}

func (b *Boolean) LiteralRepr() string {
	return b.Token.Literal
}

func (b *Boolean) Type() string {
	return "bool"
}
