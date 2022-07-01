package analysis

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strings"
)

var BinaryOps = GetBuiltinBinaryOperators()
var UnaryOps = GetBuiltinUnaryOperators()

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
	case *ast.PromotedExpr:
		tc.TypeCheck(actual.Expr)
	case *ast.Block:
		tc.TypeCheckBlock(actual)
	case *ast.BinaryOp:
		tc.TypeCheckBinaryOp(actual)
	case *ast.UnaryOp:
		tc.TypeCheckUnaryOp(actual)
	case *ast.IfConditional:
		tc.TypeCheckIfExpr(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit:
		// do nothing
	default:
		shared.ReportError("TypeChecker", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckIfExpr(cond *ast.IfConditional) {
	tc.TypeCheck(cond.Condition)

	if cond.Condition.Type().TypeSignature() != "bool" {
		// unexpected return type
		var msg strings.Builder
		line := cond.Start.TokenSpan.Line
		col := cond.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Condition for If expression should be typed boolean")
		msg.WriteString(" but got " + cond.Condition.Type().TypeSignature() + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}

	tc.TypeCheckBlock(cond.ThenBody)

	if cond.ElseBody != nil {
		tc.TypeCheck(cond.ElseBody)

		if cond.ThenBody.Type().TypeSignature() != cond.ElseBody.Type().TypeSignature() {
			// unexpected return type
			var msg strings.Builder
			line := cond.Start.TokenSpan.Line
			col := cond.Start.TokenSpan.Col
			then := cond.ThenBody.BlockType.TypeSignature()
			else_ := cond.ElseBody.Type().TypeSignature()
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("If block is typed " + then + " and Else block is typed " + else_ + ".")
			msg.WriteString("Please fix inconsistent types.")
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	}
	cond.IfType = cond.ThenBody.BlockType
}

func (tc *TypeChecker) TypeCheckUnaryOp(uop *ast.UnaryOp) {
	tc.TypeCheck(uop.Operand)
	found_op := false
Loop:
	for _, allowed := range UnaryOps {
		returns, ok := allowed.AllowsBinding(uop.Operator.Literal, uop.Operand.Type())
		if ok {
			switch returns {
			case "i64":
				uop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.I64_TYPE,
						Literal: "i64",
						TokenSpan: lexer.Span{
							Line:  uop.Operator.TokenSpan.Line,
							Col:   uop.Operator.TokenSpan.Col,
							Start: uop.Operator.TokenSpan.Start,
							End:   uop.Operator.TokenSpan.End,
						},
					},
				}
			case "bool":
				uop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.BOOL_TYPE,
						Literal: "bool",
						TokenSpan: lexer.Span{
							Line:  uop.Operator.TokenSpan.Line,
							Col:   uop.Operator.TokenSpan.Col,
							Start: uop.Operator.TokenSpan.Start,
							End:   uop.Operator.TokenSpan.End,
						},
					},
				}
			default:
				// unexpected return type
				var msg strings.Builder
				line := uop.Operator.TokenSpan.Line
				col := uop.Operator.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("Unexpected return type: " + returns)
				shared.ReportError("TypeChecker", msg.String())
				tc.FoundError = true
				break Loop
			}
			found_op = true
		}
	}

	if !found_op {
		// incorrect unary operation
		var msg strings.Builder
		line := uop.Operator.TokenSpan.Line
		col := uop.Operator.TokenSpan.Col
		oprator := uop.Operand.Type().TypeSignature()
		op := uop.Operator.Literal
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s %s is not an allowed unary expression.", op, oprator))
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckBinaryOp(binop *ast.BinaryOp) {
	tc.TypeCheck(binop.Left)
	tc.TypeCheck(binop.Right)
	found_op := false
Loop:
	for _, allowed := range BinaryOps {
		returns, ok := allowed.AllowsBinding(binop.Operator.Literal, binop.Left.Type(), binop.Right.Type())
		if ok {
			switch returns {
			case "i64":
				binop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.I64_TYPE,
						Literal: "i64",
						TokenSpan: lexer.Span{
							Line:  binop.Operator.TokenSpan.Line,
							Col:   binop.Operator.TokenSpan.Col,
							Start: binop.Operator.TokenSpan.Start,
							End:   binop.Operator.TokenSpan.End,
						},
					},
				}
			case "char":
				binop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.CHAR_TYPE,
						Literal: "char",
						TokenSpan: lexer.Span{
							Line:  binop.Operator.TokenSpan.Line,
							Col:   binop.Operator.TokenSpan.Col,
							Start: binop.Operator.TokenSpan.Start,
							End:   binop.Operator.TokenSpan.End,
						},
					},
				}
			case "bool":
				binop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.BOOL_TYPE,
						Literal: "bool",
						TokenSpan: lexer.Span{
							Line:  binop.Operator.TokenSpan.Line,
							Col:   binop.Operator.TokenSpan.Col,
							Start: binop.Operator.TokenSpan.Start,
							End:   binop.Operator.TokenSpan.End,
						},
					},
				}
			case "str":
				binop.Op_Type = &ast.Proto_Builtin{
					TypeToken: lexer.ProtoToken{
						Type:    lexer.STRING_TYPE,
						Literal: "str",
						TokenSpan: lexer.Span{
							Line:  binop.Operator.TokenSpan.Line,
							Col:   binop.Operator.TokenSpan.Col,
							Start: binop.Operator.TokenSpan.Start,
							End:   binop.Operator.TokenSpan.End,
						},
					},
				}
			default:
				// unexpected return type
				var msg strings.Builder
				line := binop.Operator.TokenSpan.Line
				col := binop.Operator.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("Unexpected return type: " + returns)
				shared.ReportError("TypeChecker", msg.String())
				tc.FoundError = true
				break Loop
			}
			found_op = true
		}
	}

	if !found_op {
		// incorrect binary operation
		var msg strings.Builder
		line := binop.Operator.TokenSpan.Line
		col := binop.Operator.TokenSpan.Col
		left := binop.Left.Type().TypeSignature()
		right := binop.Right.Type().TypeSignature()
		op := binop.Operator.Literal
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s %s %s is not an allowed binary expression.", left, op, right))
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckBlock(block *ast.Block) {
	var block_type ast.ProtoType = block.BlockType
	if !strings.Contains(block_type.TypeSignature(), "untyped") {
		return
	}

	for index, node := range block.Contents {
		tc.TypeCheck(node)

		if index+1 == len(block.Contents) {
			// we have typechecked the last node in the block
			// so we can set block_type to its type
			switch actual := node.(type) {
			case ast.Expression:
				block_type = actual.Type()
			default:
				block_type = &ast.Proto_Unit{}
			}
		}
	}
	block.BlockType = block_type
}

func (tc *TypeChecker) TypeCheckTuple(tuple *ast.Tuple) {
	for index, item := range tuple.Items {
		tc.TypeCheck(item)

		if index >= len(tuple.TupleType.InternalTypes) {
			var msg strings.Builder
			line := tuple.Token.TokenSpan.Line
			col := tuple.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("assigned tuple has more items than annotated type.")
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			break
		} else {
			tuple.TupleType.InternalTypes[index] = item.Type()
		}
	}
}

func (tc *TypeChecker) TypeCheckArray(arr *ast.Array) {
	if !strings.Contains(arr.Type().TypeSignature(), "untyped") {
		return
	}

	var arr_type *ast.Proto_Array = nil
	for _, item := range arr.Items {
		tc.TypeCheck(item)

		if arr_type == nil { // first time
			arr_type = &ast.Proto_Array{
				InternalType: item.Type(),
			}
		} else if arr_type.InternalType.TypeSignature() != item.Type().TypeSignature() {
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

	if arr_type == nil {
		arr_type = &ast.Proto_Array{
			InternalType: &ast.Proto_Unit{},
		}
	}
	arr.ArrayType = arr_type
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
