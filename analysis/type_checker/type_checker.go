package analysis

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strings"
)

type TypeChecker struct {
	TypeEnvs   []map[string]ast.ProtoType
	FoundError bool
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		TypeEnvs:   []map[string]ast.ProtoType{},
		FoundError: false,
	}
}

func (tc *TypeChecker) EnterTypeEnv() {
	newEnv := make(map[string]ast.ProtoType)
	tc.TypeEnvs = append(tc.TypeEnvs, newEnv)
}

func (tc *TypeChecker) ExitTypeEnv() {
	tc.TypeEnvs = tc.TypeEnvs[:len(tc.TypeEnvs)-1]
}

func (tc *TypeChecker) topenv() map[string]ast.ProtoType {
	return tc.TypeEnvs[len(tc.TypeEnvs)-1]
}

func (tc *TypeChecker) SetTypeForName(name lexer.ProtoToken, proto_type ast.ProtoType) {
	if len(tc.TypeEnvs) == 0 {
		return
	}
	env := tc.topenv()
	env[name.Literal] = proto_type
}

func (tc *TypeChecker) GetTypeForName(name lexer.ProtoToken) ast.ProtoType {
	var proto_type ast.ProtoType = nil
	for i := len(tc.TypeEnvs) - 1; i >= 0; i-- {
		env := tc.TypeEnvs[i]
		if found_type, ok := env[name.Literal]; ok {
			proto_type = found_type
			break
		}
	}

	if proto_type == nil {
		var msg strings.Builder
		line := name.TokenSpan.Line
		col := name.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(name.Literal + " has not been declared.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
	return proto_type
}

func (tc *TypeChecker) ContainsIdent(ident string) bool {
	for i := len(tc.TypeEnvs) - 1; i >= 0; i-- {
		env := tc.TypeEnvs[i]
		if _, ok := env[ident]; ok {
			return ok
		}
	}
	return false
}

func (tc *TypeChecker) TypeCheckProgram(prog *ast.ProtoProgram) {
	tc.EnterTypeEnv()
	for _, node := range prog.Contents {
		tc.TypeCheck(node)
	}
	tc.ExitTypeEnv()
}

func (tc *TypeChecker) TypeCheck(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.VariableDecl:
		tc.TypeCheckVariableDecl(actual)
	case *ast.Identifier:
		tc.TypeCheckIdentifier(actual)
	case *ast.Tuple:
		tc.TypeCheckTuple(actual)
	case *ast.Array:
		tc.TypeCheckArray(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit:
	default:
		shared.ReportError("TypeChecker", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
	}
}

func (tc *TypeChecker) TypeCheckTuple(tuple *ast.Tuple) {
	for index, item := range tuple.Items {
		tc.TypeCheck(item)
		tuple.TupleType.InternalTypes[index] = item.Type()
	}
}

func (tc *TypeChecker) TypeCheckArray(arr *ast.Array) {
	if !strings.Contains(arr.Type().TypeSignature(), "untyped") {
		return
	}

	var arr_type ast.ProtoType = nil
	for _, item := range arr.Items {
		tc.TypeCheck(item)

		if arr_type == nil { // first time
			arr_type = item.Type()
		} else if arr_type.TypeSignature() != item.Type().TypeSignature() {
			var msg strings.Builder
			line := arr.Token.TokenSpan.Line
			col := arr.Token.TokenSpan.Col
			expected := arr_type.TypeSignature()
			actual := item.Type().TypeSignature()
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("array should only contain " + expected + " type ")
			msg.WriteString("but included value of type " + actual + ".")
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			break
		}
	}
}

func (tc *TypeChecker) TypeCheckIdentifier(ident *ast.Identifier) {
	if !tc.ContainsIdent(ident.LiteralRepr()) {
		var msg strings.Builder
		line := ident.Token.TokenSpan.Line
		col := ident.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Undefined variable '" + ident.LiteralRepr() + "'")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	} else {
		ident.Id_Type = tc.GetTypeForName(ident.Token)
	}
}

func (tc *TypeChecker) TypeCheckVariableDecl(var_def *ast.VariableDecl) {
	tc.TypeCheck(var_def.Assigned)

	if strings.Contains(var_def.VarType.TypeSignature(), "untyped") {
		// not already inferred
		var_def.VarType = var_def.Assigned.Type()
	} else if var_def.VarType.TypeSignature() != var_def.Assigned.Type().TypeSignature() {
		var msg strings.Builder
		line := var_def.Assignee.Token.TokenSpan.Line
		col := var_def.Assignee.Token.TokenSpan.Col
		literal := var_def.Assignee.LiteralRepr()
		expected := var_def.VarType.TypeSignature()
		actual := var_def.Assigned.Type().TypeSignature()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(literal + " is annotated as " + expected + " type ")
		msg.WriteString("but is assigned value of type " + actual + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
	tc.SetTypeForName(var_def.Assignee.Token, var_def.VarType)
}
