package ast

import (
	"proto/lexer"
	"strings"
)

type ProtoType interface {
	TypeSignature() string
	CppTypeSignature() string
}

type Proto_EmptyArray struct{}

func (t *Proto_EmptyArray) TypeSignature() string {
	return "empty_array"
}

func (t *Proto_EmptyArray) CppTypeSignature() string {
	return "empty_array"
}

type Proto_Untyped struct{}

func (t *Proto_Untyped) TypeSignature() string {
	return "untyped"
}

func (t *Proto_Untyped) CppTypeSignature() string {
	return "auto"
}

type Proto_Builtin struct {
	TypeToken lexer.ProtoToken
}

func (t *Proto_Builtin) TypeSignature() string {
	return t.TypeToken.Literal
}

func (t *Proto_Builtin) CppTypeSignature() string {
	var _type string
	switch t.TypeToken.Literal {
	case "i64":
		_type = "int64_t"
	case "bool":
		_type = "bool"
	case "char":
		_type = "char"
	case "str":
		_type = "string"
	}
	return _type
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

func (t *Proto_Array) CppTypeSignature() string {
	var str strings.Builder
	str.WriteString("vector<")
	str.WriteString(t.InternalType.CppTypeSignature())
	str.WriteString(">")

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

func (t *Proto_Tuple) CppTypeSignature() string {
	var types strings.Builder

	types.WriteString("tuple <")
	for index, i_type := range t.InternalTypes {
		types.WriteString(i_type.CppTypeSignature())

		if index+1 < len(t.InternalTypes) {
			types.WriteString(", ")
		}
	}
	types.WriteString(">")
	return types.String()
}

type Proto_UserDef struct {
	Name       *Identifier
	Definition *Struct
}

func (u *Proto_UserDef) TypeSignature() string {
	return u.Name.LiteralRepr()
}

func (u *Proto_UserDef) CppTypeSignature() string {
	return u.Name.LiteralRepr()
}

type Proto_Range struct {
	InternalType     ProtoType
	IsInclusiveRange bool
}

func (r *Proto_Range) TypeSignature() string {
	return "Range<" + r.InternalType.TypeSignature() + ">"
}

func (r *Proto_Range) CppTypeSignature() string {
	if r.IsInclusiveRange {
		return "IRange"
	}
	return "Range"
}

type Proto_Unit struct{}

func (u *Proto_Unit) TypeSignature() string {
	return "()"
}

func (u *Proto_Unit) CppTypeSignature() string {
	return "Proto_Unit"
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

func (f *Proto_Function) CppTypeSignature() string {
	return "fn" + f.Params.TypeSignature() +
		" -> " + f.Return.TypeSignature()
}

type Proto_Reference struct {
	Inner ProtoType
}

func (r *Proto_Reference) TypeSignature() string {
	return "&" + r.Inner.TypeSignature()
}

func (r *Proto_Reference) CppTypeSignature() string {
	return "&" + r.Inner.CppTypeSignature()
}

type Proto_Variad struct {
	Inner ProtoType
}

func (v *Proto_Variad) TypeSignature() string {
	return "..." + v.Inner.TypeSignature()
}

func (v *Proto_Variad) CppTypeSignature() string {
	return v.TypeSignature()
}
