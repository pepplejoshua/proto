package type_checker

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strconv"
	"strings"
)

var BinaryOps = GetBuiltinBinaryOperators()
var UnaryOps = GetBuiltinUnaryOperators()

type BlockType int

const (
	NONE BlockType = iota
	IF_EXPR
	FUNCTION
	LOOP
)

type TypeChecker struct {
	TypeEnvs   []map[string]ast.ProtoType
	FoundError bool
	// these 2 are used for typechecking function calls,
	// vs updating every function to take a flag.
	CurReturnType ast.ProtoType
	FnDefSpan     *lexer.Span
	CurBlockType  BlockType
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		TypeEnvs:      []map[string]ast.ProtoType{},
		FoundError:    false,
		CurReturnType: nil,
		FnDefSpan:     nil,
		CurBlockType:  NONE,
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

	for _, s := range prog.Structs {
		tc.SetTypeForName(s.Name.Token, &ast.Proto_UserDef{
			Name:       &s.Name,
			Definition: s,
		})
	}

	for _, fn := range prog.FunctionDefs {
		tc.SetTypeForName(fn.Name.Token, fn.FunctionTypeSignature)
	}

	for _, node := range prog.Contents {
		tc.TypeCheck(node)
	}
	tc.ExitTypeEnv()
}

func (tc *TypeChecker) TypeCheck(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.VariableDecl:
		tc.TypeCheckVariableDecl(actual)
	case *ast.Struct:
		tc.TypeCheckStruct(actual)
	case *ast.StructInitialization:
		tc.TypeCheckStructInit(actual)
	case *ast.Identifier:
		tc.TypeCheckIdentifier(actual)
	case *ast.Tuple:
		tc.TypeCheckTuple(actual)
	case *ast.Array:
		tc.TypeCheckArray(actual)
	case *ast.PromotedExpr:
		tc.TypeCheck(actual.Expr)
	case *ast.Block:
		tc.TypeCheckBlock(actual, true)
	case *ast.BinaryOp:
		tc.TypeCheckBinaryOp(actual)
	case *ast.UnaryOp:
		tc.TypeCheckUnaryOp(actual)
	case *ast.IfConditional:
		tc.TypeCheckIfExpr(actual)
	case *ast.WhileLoop:
		tc.TypeCheckWhileLoop(actual)
	case *ast.InfiniteLoop:
		tc.TypeCheck(actual.Body)
	case *ast.GenericForLoop:
		tc.TypeCheckGenericFor(actual)
	case *ast.CollectionsForLoop:
		tc.TypeCheckCollectionsFor(actual)
	case *ast.Range:
		tc.TypeCheckRange(actual)
	case *ast.InclusiveRange:
		tc.TypeCheckInclusiveRange(actual)
	case *ast.IndexExpression:
		tc.TypeCheckIndexExpression(actual)
	case *ast.Assignment:
		tc.TypeCheckAssignment(actual)
	case *ast.FunctionDef:
		tc.TypeCheckFunctionDef(actual)
	case *ast.CallExpression:
		tc.TypeCheckCallExpr(actual)
	case *ast.Membership:
		tc.TypeCheckMembership(actual)
	case *ast.Return:
		tc.TypeCheckReturn(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit, *ast.Break, *ast.Continue:
		// do nothing
	default:
		shared.ReportError("TypeChecker", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckReturn(ret *ast.Return) {
	if ret.Value != nil {
		tc.TypeCheck(ret.Value)

		if ret.Value.Type().TypeSignature() != tc.CurReturnType.TypeSignature() {
			var msg strings.Builder
			line := ret.Token.TokenSpan.Line
			col := ret.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("return value \"%s\" has type %s does not match function return type, which is %s.",
				ret.Value.LiteralRepr(), ret.Value.Type().TypeSignature(),
				tc.CurReturnType.TypeSignature()))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
		}
	} else {
		if tc.CurReturnType.TypeSignature() != "()" {
			// check if function return type is unit
			var msg strings.Builder
			line := ret.Token.TokenSpan.Line
			col := ret.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("returning () does not match function return type, which is %s.",
				tc.CurReturnType.TypeSignature()))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
		}
	}
}

func (tc *TypeChecker) TypeCheckCallExpr(call *ast.CallExpression) {
	tc.TypeCheck(call.Callable)

	switch actual := call.Callable.Type().(type) {
	case *ast.Proto_Function:
		call.ReturnType = actual.Return
	default:
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s type cannot be called.", actual.TypeSignature()))
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}
}

func (tc *TypeChecker) TypeCheckFunctionDef(fn *ast.FunctionDef) {
	tc.SetTypeForName(fn.Name.Token, fn.FunctionTypeSignature)
	tc.EnterTypeEnv()
	for _, param := range fn.ParameterList {
		tc.SetTypeForName(param.Token, param.Id_Type)
	}
	prevFnDefSpan := tc.FnDefSpan
	prevCurReturnType := tc.CurReturnType
	tc.FnDefSpan = &fn.Start.TokenSpan
	tc.CurReturnType = fn.ReturnType
	prev := tc.CurBlockType
	tc.CurBlockType = FUNCTION
	tc.TypeCheckBlock(fn.Body, false)
	tc.CurBlockType = prev
	tc.FnDefSpan = prevFnDefSpan
	tc.CurReturnType = prevCurReturnType
	tc.ExitTypeEnv()
}

func (tc *TypeChecker) TypeCheckMembership(mem *ast.Membership) {
	tc.TypeCheck(mem.Object)

	member := mem.Member
	switch actual := mem.Object.Type().(type) {
	case *ast.Proto_UserDef:
		switch ref := member.(type) {
		case *ast.Identifier:
			found := false
			fetched := tc.GetTypeForName(actual.Name.Token).(*ast.Proto_UserDef)
			for _, actual_mem := range fetched.Definition.Members {
				if ref.LiteralRepr() == actual_mem.LiteralRepr() {
					mem.MembershipType = actual_mem.Type()
					found = true
					return
				}
			}
			if !found {
				var msg strings.Builder
				line := mem.Start.TokenSpan.Line
				col := mem.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("%s is not a member of %s Struct.",
					ref.LiteralRepr(), actual.Name.LiteralRepr()))
				shared.ReportError("TypeChecker", msg.String())
				tc.FoundError = true
			}
		default:
			var msg strings.Builder
			line := mem.Start.TokenSpan.Line
			col := mem.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Expected an identifier to access member of Struct but got %s, which is of type %s.",
				mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
		}
	case *ast.Proto_Tuple:
		switch ref := member.(type) {
		case *ast.I64:
			num, _ := strconv.ParseInt(ref.LiteralRepr(), 10, 64)
			tup_len := len(actual.InternalTypes)
			if num >= int64(tup_len) {
				var msg strings.Builder
				line := mem.Start.TokenSpan.Line
				col := mem.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("Tuple has %d item (indexed from 0 to %d), so provided index(%d) is out of range.",
					tup_len, tup_len-1, num))
				tc.FoundError = true
			}
			mem.MembershipType = actual.InternalTypes[num]
		default:
			var msg strings.Builder
			line := mem.Start.TokenSpan.Line
			col := mem.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Expected an i64 to access slot in Tuple but got %s, which is of type %s.",
				mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
		}

	default:
		var msg strings.Builder
		line := mem.Start.TokenSpan.Line
		col := mem.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Expected a Struct type as target for membership operation but got %s, which is of type %s.",
			mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckStructInit(i *ast.StructInitialization) {
	// actual := tc.GetTypeForName(i.StructName.Token)
	actual := i.Type()

	switch sdef := actual.(type) {
	case *ast.Proto_UserDef:
		def := sdef.Definition
		for _, mem := range def.Members {
			for id, expr := range i.Fields {
				if id.LiteralRepr() == mem.LiteralRepr() {
					tc.TypeCheck(expr)
					if mem.Id_Type.TypeSignature() != expr.Type().TypeSignature() {
						var msg strings.Builder
						line := id.Token.TokenSpan.Line
						col := id.Token.TokenSpan.Col
						msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
						msg.WriteString(fmt.Sprintf("%s.%s is defined as %s type but was initialized with value of type %s.",
							i.StructName.LiteralRepr(), mem.LiteralRepr(), mem.Id_Type.TypeSignature(),
							expr.Type().TypeSignature()))
						shared.ReportError("TypeChecker", msg.String())
						tc.FoundError = true
					}

				}
			}
		}
		i.StructType.Definition = sdef.Definition
	}
}

func (tc *TypeChecker) TypeCheckStruct(s *ast.Struct) {
	tc.SetTypeForName(s.Name.Token, &ast.Proto_UserDef{
		Name:       &s.Name,
		Definition: s,
	})
}

func (tc *TypeChecker) TypeCheckAssignment(assign *ast.Assignment) {
	tc.TypeCheck(assign.Assigned)
	tc.TypeCheck(assign.Target)
	assigned := assign.Assigned
	target := assign.Target

	switch assign.AssignmentToken.Literal {
	case lexer.ASSIGN:
		if target.Type().TypeSignature() != assigned.Type().TypeSignature() {
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The target of the assignment %s is typed %s but was assigned value of type %s.",
				target.LiteralRepr(), target.Type().TypeSignature(), assigned.Type().TypeSignature()))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	case lexer.PLUS_EQUAL:
		switch target.Type().TypeSignature() {
		case "i64":
			switch assigned.Type().TypeSignature() {
			case "i64":
			default:
				var msg strings.Builder
				line := assign.AssignmentToken.TokenSpan.Line
				col := assign.AssignmentToken.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("The assigned value %s is typed %s and cannot use the %s operator with a target of type %s.",
					assigned.LiteralRepr(), assigned.Type().TypeSignature(), assign.AssignmentToken.Literal, target.Type().TypeSignature()))
				shared.ReportError("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
		case "str":
			switch assigned.Type().TypeSignature() {
			case "char":
			case "str":
			default:
				var msg strings.Builder
				line := assign.AssignmentToken.TokenSpan.Line
				col := assign.AssignmentToken.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("The assigned value %s is typed %s and cannot use the %s operator with a target of type %s.",
					assigned.LiteralRepr(), assigned.Type().TypeSignature(), assign.AssignmentToken.Literal, target.Type().TypeSignature()))
				shared.ReportError("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
		default:
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The target of the assignment %s is typed %s and cannot use the %s operator reserved for i64|str values.",
				target.LiteralRepr(), target.Type().TypeSignature(), assign.AssignmentToken.Literal))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	case lexer.MINUS_EQUAL, lexer.STAR_EQUAL,
		lexer.SLASH_EQUAL, lexer.MODULO_EQUAL:
		if target.Type().TypeSignature() != "i64" {
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The target of the assignment %s is typed %s and cannot use the %s operator reserved for i64 values.",
				target.LiteralRepr(), target.Type().TypeSignature(), assign.AssignmentToken.Literal))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}

		if assigned.Type().TypeSignature() != "i64" {
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The assigned value %s is typed %s and cannot use the %s operator reserved for i64  values.",
				assigned.LiteralRepr(), assigned.Type().TypeSignature(), assign.AssignmentToken.Literal))
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	}
}

func (tc *TypeChecker) TypeCheckIndexExpression(index *ast.IndexExpression) {
	tc.TypeCheck(index.Indexable)
	switch actual := index.Indexable.Type().(type) {
	case *ast.Proto_Array:
		tc.TypeCheck(index.Index)
		if index.Index.Type().TypeSignature() != "i64" {
			// can only index with i64
			var msg strings.Builder
			line := index.Start.TokenSpan.Line
			col := index.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("Arrays take index of type i64, got " + index.Indexable.Type().TypeSignature() + " instead.")
			shared.ReportError("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
		// for some reason, it hold on to the struct without its
		// necessary contents so I have to do this to provide
		// the necessary items
		index.ValueType = actual.InternalType
		switch a := actual.InternalType.(type) {
		case *ast.Proto_UserDef:
			val := tc.GetTypeForName(a.Name.Token)
			index.ValueType = val
		}
	case *ast.Proto_Tuple:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Indexing a Tuple is not allowed. Use membership indexing instead (e.g tuple.0).")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	case *ast.Proto_Range:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Indexing a Range is not allowed.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	default:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Indexing is not allowed for the %s type.", actual.TypeSignature()))
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}
}

func (tc *TypeChecker) TypeCheckInclusiveRange(ir *ast.InclusiveRange) {
	tc.TypeCheck(ir.Start)
	tc.TypeCheck(ir.End)

	// ranges can currently only be i64 or characters
	if ir.Start.Type().TypeSignature() != "i64" && ir.Start.Type().TypeSignature() != "char" {
		var msg strings.Builder
		line := ir.Operator.TokenSpan.Line
		col := ir.Operator.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Ranges can only include I64 type or CHAR type, but got ")
		msg.WriteString(ir.Start.Type().TypeSignature() + " type as start value of range.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}

	// ranges can currently only be i64 or characters
	if ir.End.Type().TypeSignature() != "i64" && ir.End.Type().TypeSignature() != "char" {
		var msg strings.Builder
		line := ir.Operator.TokenSpan.Line
		col := ir.Operator.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Ranges can only include I64 type or CHAR type, but got ")
		msg.WriteString(ir.Start.Type().TypeSignature() + " type as past end value of range.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	if ir.Start.Type().TypeSignature() != ir.End.Type().TypeSignature() {
		var msg strings.Builder
		line := ir.Operator.TokenSpan.Line
		col := ir.Operator.TokenSpan.Col
		start := ir.Start.Type().TypeSignature()
		past_end := ir.End.Type().TypeSignature()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Expected both types in Range to be the same, but got " + start + ".." + past_end + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	ir.RangeType.InternalType = ir.Start.Type()
}

func (tc *TypeChecker) TypeCheckRange(r *ast.Range) {
	tc.TypeCheck(r.Start)
	tc.TypeCheck(r.PastEnd)

	// ranges can currently only be i64 or characters
	if r.Start.Type().TypeSignature() != "i64" && r.Start.Type().TypeSignature() != "char" {
		var msg strings.Builder
		line := r.Operator.TokenSpan.Line
		col := r.Operator.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Ranges can only include I64 type or CHAR type, but got ")
		msg.WriteString(r.Start.Type().TypeSignature() + " type as start value of range.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}

	// ranges can currently only be i64 or characters
	if r.PastEnd.Type().TypeSignature() != "i64" && r.PastEnd.Type().TypeSignature() != "char" {
		var msg strings.Builder
		line := r.Operator.TokenSpan.Line
		col := r.Operator.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Ranges can only include I64 type or CHAR type, but got ")
		msg.WriteString(r.Start.Type().TypeSignature() + " type as past end value of range.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	if r.Start.Type().TypeSignature() != r.PastEnd.Type().TypeSignature() {
		var msg strings.Builder
		line := r.Operator.TokenSpan.Line
		col := r.Operator.TokenSpan.Col
		start := r.Start.Type().TypeSignature()
		past_end := r.PastEnd.Type().TypeSignature()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Expected both types in Range to be the same, but got " + start + ".." + past_end + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	r.RangeType.InternalType = r.Start.Type()
}

func (tc *TypeChecker) TypeCheckCollectionsFor(col_for *ast.CollectionsForLoop) {
	tc.TypeCheck(col_for.Collection)

	switch actual := col_for.Collection.Type().(type) {
	case *ast.Proto_Array:
		tc.EnterTypeEnv()
		tc.SetTypeForName(col_for.LoopVar.Token, actual.InternalType)
		prev := tc.CurBlockType
		tc.CurBlockType = LOOP
		tc.TypeCheckBlock(col_for.Body, false)
		tc.CurBlockType = prev
		tc.ExitTypeEnv()
	case *ast.Proto_Range:
		tc.EnterTypeEnv()
		tc.SetTypeForName(col_for.LoopVar.Token, actual.InternalType)
		prev := tc.CurBlockType
		tc.CurBlockType = LOOP
		tc.TypeCheckBlock(col_for.Body, false)
		tc.CurBlockType = prev
		tc.ExitTypeEnv()
	case *ast.Proto_Tuple:
		var msg strings.Builder
		line := col_for.Start.TokenSpan.Line
		col := col_for.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Collections looping through a Tuple is not allowed ")
		msg.WriteString("as types may not be consistent. Try an array if types are consistent.")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	default:
		var msg strings.Builder
		line := col_for.Start.TokenSpan.Line
		col := col_for.Start.TokenSpan.Col
		type_ := col_for.Collection.Type().TypeSignature()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Collections looping through a %s is not allowed. ", type_))
		msg.WriteString("Only arrays and ranges are allowed (for now).")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}
}

func (tc *TypeChecker) TypeCheckGenericFor(loop *ast.GenericForLoop) {
	tc.EnterTypeEnv()
	tc.TypeCheckVariableDecl(loop.Init)
	tc.TypeCheck(loop.LoopCondition)
	if loop.LoopCondition.Type().TypeSignature() != "bool" {
		var msg strings.Builder
		line := loop.Start.TokenSpan.Line
		col := loop.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Loop Condition for while loop should be of type boolean")
		msg.WriteString(" but got " + loop.LoopCondition.Type().TypeSignature() + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
	tc.TypeCheck(loop.Update)
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlock(loop.Body, false)
	tc.CurBlockType = prev
	tc.ExitTypeEnv()
}

func (tc *TypeChecker) TypeCheckWhileLoop(loop *ast.WhileLoop) {
	tc.TypeCheck(loop.LoopCondition)

	if loop.LoopCondition.Type().TypeSignature() != "bool" {
		var msg strings.Builder
		line := loop.Start.TokenSpan.Line
		col := loop.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Loop Condition for while loop should be of type boolean")
		msg.WriteString(" but got " + loop.LoopCondition.Type().TypeSignature() + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlock(loop.Body, false)
	tc.CurBlockType = prev
}

func (tc *TypeChecker) TypeCheckIfExpr(cond *ast.IfConditional) {
	tc.TypeCheck(cond.Condition)

	if cond.Condition.Type().TypeSignature() != "bool" {
		var msg strings.Builder
		line := cond.Start.TokenSpan.Line
		col := cond.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Condition for If expression should be of type boolean")
		msg.WriteString(" but got " + cond.Condition.Type().TypeSignature() + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
	}

	tc.EnterTypeEnv()
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlock(cond.ThenBody, false)
	tc.CurBlockType = prev
	tc.ExitTypeEnv()

	if cond.ElseBody != nil {
		prev := tc.CurBlockType
		tc.CurBlockType = LOOP
		tc.TypeCheck(cond.ElseBody)
		tc.CurBlockType = prev

		if cond.ThenBody.Type().TypeSignature() != cond.ElseBody.Type().TypeSignature() {
			// unexpected return type
			var msg strings.Builder
			line := cond.Start.TokenSpan.Line
			col := cond.Start.TokenSpan.Col
			then := cond.ThenBody.BlockType.TypeSignature()
			else_ := cond.ElseBody.Type().TypeSignature()
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("If block is typed " + then + " and Else block is typed " + else_ + ". ")
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
			break
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
			break
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

func (tc *TypeChecker) TypeCheckBlock(block *ast.Block, new_env bool) {
	var block_type ast.ProtoType = block.BlockType
	if new_env {
		tc.EnterTypeEnv()
	}

	for index, node := range block.Contents {
		tc.TypeCheck(node)

		if index+1 == len(block.Contents) {
			// we have typechecked the last node in the block
			// so we can set block_type to its type
			switch actual := node.(type) {
			case ast.Expression:
				if tc.CurReturnType != nil && tc.CurBlockType == FUNCTION {
					// we are in a function
					if actual.Type().TypeSignature() != tc.CurReturnType.TypeSignature() {
						var msg strings.Builder
						line := tc.FnDefSpan.Line
						col := tc.FnDefSpan.Col
						msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
						msg.WriteString(fmt.Sprintf("implicit return type %s does not match function return type, which is %s.",
							actual.Type().TypeSignature(), tc.CurReturnType.TypeSignature()))
						shared.ReportError("TypeChecker", msg.String())
						tc.FoundError = true
					}
				}
				block_type = actual.Type()
			case *ast.Return:
				// do nothing, since it has been checked
			default:
				if tc.CurReturnType != nil {
					// we are in a function
					if tc.CurReturnType.TypeSignature() != "()" {
						var msg strings.Builder
						line := tc.FnDefSpan.Line
						col := tc.FnDefSpan.Col
						msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
						msg.WriteString(fmt.Sprintf("implicit return type () does not match function return type, which is %s.",
							tc.CurReturnType.TypeSignature()))
						shared.ReportError("TypeChecker", msg.String())
						tc.FoundError = true
					}
				}
				block_type = &ast.Proto_Unit{}
			}
		}
	}
	block.BlockType = block_type
	if new_env {
		tc.ExitTypeEnv()
	}
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
	var arr_type *ast.Proto_Array
	if !strings.Contains(arr.ArrayType.TypeSignature(), "empty_array") &&
		!strings.Contains(arr.ArrayType.TypeSignature(), "untyped") {
		arr_type = arr.ArrayType
	} else {
		arr_type = nil
	}

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
			InternalType: &ast.Proto_EmptyArray{},
		}
		var msg strings.Builder
		line := arr.Token.TokenSpan.Line
		col := arr.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("empty array cannot be type checked. Consider annotating with [_type_;]")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
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

	if strings.Contains(var_def.VarType.TypeSignature(), "untyped") &&
		!strings.Contains(var_def.Assigned.Type().TypeSignature(), "untyped") {
		// not already inferred
		var_def.VarType = var_def.Assigned.Type()
	} else if strings.Contains(var_def.VarType.TypeSignature(), "untyped") &&
		strings.Contains(var_def.Assigned.Type().TypeSignature(), "untyped") {
		var msg strings.Builder
		line := var_def.Assignee.Token.TokenSpan.Line
		col := var_def.Assignee.Token.TokenSpan.Col
		literal := var_def.Assignee.LiteralRepr()
		actual := var_def.Assigned.Type().TypeSignature()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(literal + " is not annotated and cannot infer type for " + actual + ".")
		shared.ReportError("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	if var_def.VarType.TypeSignature() != var_def.Assigned.Type().TypeSignature() {
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
		return
	}
	tc.SetTypeForName(var_def.Assignee.Token, var_def.VarType)
}
