package ast

import (
	"proto/lexer"
	"strings"
)

type ProtoType interface {
	TypeSignature() string
}

type Proto_EmptyArray struct{}

func (t *Proto_EmptyArray) TypeSignature() string {
	return "empty_array"
}

type Proto_Untyped struct{}

func (t *Proto_Untyped) TypeSignature() string {
	return "untyped"
}

type Proto_Builtin struct {
	TypeToken lexer.ProtoToken
}

func (t *Proto_Builtin) TypeSignature() string {
	return t.TypeToken.Literal
}

type Proto_Array struct {
	InternalType ProtoType
}

func (t *Proto_Array) TypeSignature() string {
	var str strings.Builder
	str.WriteString("[")
	str.WriteString(t.InternalType.TypeSignature())
	str.WriteString("]")

	return str.String()
}

type Proto_Tuple struct {
	InternalTypes []ProtoType
}

func (t *Proto_Tuple) TypeSignature() string {
	var str strings.Builder
	str.WriteString("(")

	for index, proto_type := range t.InternalTypes {
		str.WriteString(proto_type.TypeSignature())

		if index+1 < len(t.InternalTypes) {
			str.WriteString(", ")
		}
	}
	str.WriteString(")")

	return str.String()
}

type Proto_UserDef struct {
	Name       *Identifier
	Definition *Struct
}

func (u *Proto_UserDef) TypeSignature() string {
	return u.Name.LiteralRepr()
}

type Proto_Range struct {
	InternalType     ProtoType
	IsInclusiveRange bool
}

func (r *Proto_Range) TypeSignature() string {
	return "Range<" + r.InternalType.TypeSignature() + ">"
}

type Proto_Unit struct{}

func (u *Proto_Unit) TypeSignature() string {
	return "()"
}

type Proto_Function struct {
	Params *Proto_Tuple
	Return ProtoType
	Fn     *FunctionDef
}

func (f *Proto_Function) TypeSignature() string {
	return "fn" + f.Params.TypeSignature() +
		" -> " + f.Return.TypeSignature()
}

type Proto_Reference struct {
	Inner ProtoType
}

func (r *Proto_Reference) TypeSignature() string {
	return "&" + r.Inner.TypeSignature()
}
