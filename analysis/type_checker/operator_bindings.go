package analysis

import (
	"proto/ast"
)

type BinaryOpBindingPair struct {
	left     string
	right    string
	operator string
	returns  string
}

func (bp *BinaryOpBindingPair) AllowsBinding(operator string, left ast.ProtoType, right ast.ProtoType) (string, bool) {
	if left.TypeSignature() == bp.left &&
		right.TypeSignature() == bp.right &&
		operator == bp.operator {
		return bp.returns, true
	}
	return "", false
}

type UnaryOpBindingPair struct {
	operand  string
	operator string
	returns  string
}

func (bp *UnaryOpBindingPair) AllowsBinding(operator string, operand ast.ProtoType) (string, bool) {
	if operand.TypeSignature() == bp.operand && operator == bp.operator {
		return bp.returns, true
	}
	return "", false
}

func GetBuiltinUnaryOperators() []*UnaryOpBindingPair {
	var BuiltinUnaryOps []*UnaryOpBindingPair

	BuiltinUnaryOps = append(BuiltinUnaryOps, &UnaryOpBindingPair{
		operand:  "i64",
		operator: "-",
		returns:  "i64",
	})

	BuiltinUnaryOps = append(BuiltinUnaryOps, &UnaryOpBindingPair{
		operand:  "bool",
		operator: "not",
		returns:  "bool",
	})

	return BuiltinUnaryOps
}

func GetBuiltinBinaryOperators() []*BinaryOpBindingPair {
	var BuiltinBinaryOps []*BinaryOpBindingPair
	add_pairs := [][]string{
		{"i64", "i64", "i64"},
		{"char", "char", "str"},
		{"str", "str", "str"},
		{"str", "char", "str"},
	}
	for _, pair := range add_pairs {
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     pair[0],
			right:    pair[1],
			operator: "+",
			returns:  pair[2],
		})
	}

	num_ops := []string{
		"-", "*", "/", "%",
	}
	for _, op := range num_ops {
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     "i64",
			right:    "i64",
			operator: op,
			returns:  "i64",
		})
	}

	num_to_bool_ops := []string{
		"<", "<=", ">", ">=",
	}

	for _, op := range num_to_bool_ops {
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     "i64",
			right:    "i64",
			operator: op,
			returns:  "bool",
		})

		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     "char",
			right:    "char",
			operator: op,
			returns:  "bool",
		})
	}

	bool_ops := []string{"&&", "||"}
	for _, op := range bool_ops {
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     "bool",
			right:    "bool",
			operator: op,
			returns:  "bool",
		})
	}

	equality_pairs := [][]string{
		{"i64", "i64"},
		{"char", "char"},
		{"str", "str"},
		{"bool", "bool"},
	}
	for _, pair := range equality_pairs {
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     pair[0],
			right:    pair[1],
			operator: "==",
			returns:  "bool",
		})
		BuiltinBinaryOps = append(BuiltinBinaryOps, &BinaryOpBindingPair{
			left:     pair[0],
			right:    pair[1],
			operator: "!=",
			returns:  "bool",
		})
	}
	return BuiltinBinaryOps
}
