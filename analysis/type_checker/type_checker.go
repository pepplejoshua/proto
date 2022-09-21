package type_checker

import (
	"fmt"
	"proto/ast"
	"proto/builtins"
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
	ImportInfo    map[string]ast.ProtoNode
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		TypeEnvs:      []map[string]ast.ProtoType{},
		FoundError:    false,
		CurReturnType: nil,
		FnDefSpan:     nil,
		CurBlockType:  NONE,
		ImportInfo:    map[string]ast.ProtoNode{},
	}
}

func NewTypeCheckerWithImportInfo(info map[string]ast.ProtoNode) *TypeChecker {
	return &TypeChecker{
		TypeEnvs:      []map[string]ast.ProtoType{},
		FoundError:    false,
		CurReturnType: nil,
		FnDefSpan:     nil,
		CurBlockType:  NONE,
		ImportInfo:    info,
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

func (tc *TypeChecker) GetTypeForNameOrFail(name lexer.ProtoToken) ast.ProtoType {
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
	return proto_type
}

func (tc *TypeChecker) GetTypeForName(name lexer.ProtoToken) ast.ProtoType {
	for i := len(tc.TypeEnvs) - 1; i >= 0; i-- {
		env := tc.TypeEnvs[i]
		if found_type, ok := env[name.Literal]; ok {
			return found_type
		}
	}

	return nil
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
	case *ast.Reference:
		tc.TypeCheckRef(actual)
	case *ast.Dereference:
		tc.TypeCheckDeref(actual)
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
	case *ast.BlockExpr:
		tc.TypeCheckBlockExpr(actual, true)
	case *ast.BlockStmt:
		tc.TypeCheckBlockStmt(actual, true)
	case *ast.BinaryOp:
		tc.TypeCheckBinaryOp(actual)
	case *ast.UnaryOp:
		tc.TypeCheckUnaryOp(actual)
	case *ast.IfExpr:
		tc.TypeCheckIfExpr(actual)
	case *ast.IfStmt:
		tc.TypeCheckIfStmt(actual)
	case *ast.WhileLoop:
		tc.TypeCheckWhileLoop(actual)
	case *ast.InfiniteLoop:
		tc.TypeCheckInfiniteLoop(actual)
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
	case *ast.Module:
		tc.TypeCheckModule(actual)
	case *ast.UseStmt:
		tc.TypeCheckUseStmt(actual)
	case *ast.ModuleAccess:
		tc.TypeCheckModuleAccess(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit, *ast.Break, *ast.Continue, *ast.CppLiteral:
		// do nothing
	default:
		shared.ReportErrorAndExit("TypeChecker", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckModuleAccess(access *ast.ModuleAccess) {
	tc.TypeCheck(access.Mod)

	mem := access.Member

	switch ac := access.Mod.Type().(type) {
	case *ast.Proto_Module:
		mod := ac.Mod
		for _, m := range mod.Body.Modules {
			if m.Name.LiteralRepr() == mem.LiteralRepr() {
				access.MemberType = &ast.Proto_Module{
					Mod: m,
				}
				return
			}
		}

		for _, fn := range mod.Body.Functions {
			if fn.Name.LiteralRepr() == mem.LiteralRepr() {
				access.MemberType = fn.FunctionTypeSignature
				return
			}
		}

		for _, decl := range mod.Body.VariableDecls {
			if decl.Assignee.LiteralRepr() == mem.LiteralRepr() {
				access.MemberType = decl.VarType
				return
			}
		}
	}
}

func (tc *TypeChecker) TypeCheckUseStmt(use *ast.UseStmt) {
	for _, path := range use.Paths {
		last_node := path.Pieces[len(path.Pieces)-1]
		name := last_node.String()
		import_all := false
		if name == "*" {
			import_all = true
			name = path.Pieces[len(path.Pieces)-2].String()
		}

		tags := []string{}
		for i := 0; i < len(path.Pieces)-1; i++ {
			tags = append(tags, path.Pieces[i].String())
		}
		tag := strings.Join(tags, "::")
		tag += "::"
		if import_all {
			mod := tc.ImportInfo[tag+name].(*ast.Module)

			for _, m := range mod.Body.Modules {
				tc.SetTypeForName(m.Name.Token, &ast.Proto_Module{
					Mod: m,
				})
			}

			for _, fn := range mod.Body.Functions {
				tc.SetTypeForName(fn.Name.Token, fn.FunctionTypeSignature)
			}

			for _, decl := range mod.Body.VariableDecls {
				tc.SetTypeForName(decl.Assignee.Token, decl.VarType)
			}

			for _, struct_ := range mod.Body.Structs {
				tc.SetTypeForName(struct_.Name.Token, &ast.Proto_UserDef{
					Name:       &struct_.Name,
					Definition: struct_,
				})
			}
		} else {
			if len(path.Pieces) > 1 {
				if as, ok := last_node.(*ast.UseAs); ok {
					val := tc.ImportInfo[tag+as.As.LiteralRepr()]
					switch actual := val.(type) {
					case *ast.Module:
						tc.SetTypeForName(as.As.Token, &ast.Proto_Module{
							Mod: actual,
						})
					case *ast.VariableDecl:
						tc.SetTypeForName(as.As.Token, actual.VarType)
					case *ast.FunctionDef:
						tc.SetTypeForName(as.As.Token, actual.FunctionTypeSignature)
					case *ast.Struct:
						tc.SetTypeForName(as.As.Token, &ast.Proto_UserDef{
							Name:       &actual.Name,
							Definition: actual,
						})
					}
				} else {
					val := tc.ImportInfo[tag+name]
					switch actual := val.(type) {
					case *ast.Module:
						tc.SetTypeForName(actual.Name.Token, &ast.Proto_Module{
							Mod: actual,
						})
					case *ast.VariableDecl:
						tc.SetTypeForName(actual.Assignee.Token, actual.VarType)
					case *ast.FunctionDef:
						tc.SetTypeForName(actual.Name.Token, actual.FunctionTypeSignature)
					case *ast.Struct:
						tc.SetTypeForName(actual.Name.Token, &ast.Proto_UserDef{
							Name:       &actual.Name,
							Definition: actual,
						})
					}
				}
			} else {
				val := tc.ImportInfo[tag+name]
				switch actual := val.(type) {
				case *ast.Module:
					tc.SetTypeForName(actual.Name.Token, &ast.Proto_Module{
						Mod: actual,
					})
				case *ast.VariableDecl:
					tc.SetTypeForName(actual.Assignee.Token, actual.VarType)
				case *ast.FunctionDef:
					tc.SetTypeForName(actual.Name.Token, actual.FunctionTypeSignature)
				case *ast.Struct:
					tc.SetTypeForName(actual.Name.Token, &ast.Proto_UserDef{
						Name:       &actual.Name,
						Definition: actual,
					})
				}
			}
		}
	}
}

func (tc *TypeChecker) TypeCheckModule(mod *ast.Module) {
	tc.TypeCheckBlockStmt(mod.Body, true)
}

func (tc *TypeChecker) TypeCheckDeref(deref *ast.Dereference) {
	tc.TypeCheck(deref.Value)

	if ref, ok := deref.Value.Type().(*ast.Proto_Reference); !ok {
		var msg strings.Builder
		line := deref.Start.TokenSpan.Line
		col := deref.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Attempted to dereference a non-reference of type %s.", deref.Value.Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	} else {
		deref.DerefType = ref.Inner
	}

}

func (tc *TypeChecker) TypeCheckRef(ref *ast.Reference) {
	tc.TypeCheck(ref.Value)

	if _, ok := ref.Value.Type().(*ast.Proto_Reference); ok {
		var msg strings.Builder
		line := ref.Start.TokenSpan.Line
		col := ref.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("A reference to a reference is not allowed.")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
	ref.RefType.Inner = ref.Value.Type()
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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
		}
	}
}

func (tc *TypeChecker) TypeCheckBuiltinFunction(call *ast.CallExpression, builtin *builtins.BuiltinFn) {
	for _, arg := range call.Arguments {
		tc.TypeCheck(arg)
	}

	// if lengths of call expr doesnt match with builtin
	if builtin.HasTypeChecker {
		tc.FoundError = builtin.TypeChecker(builtin, call)
	}
}

func (tc *TypeChecker) TypeCheckCallExpr(call *ast.CallExpression) {
	if name, ok := call.Callable.(*ast.Identifier); ok && builtins.IsBuiltin(name.LiteralRepr()) {
		tc.TypeCheckBuiltinFunction(call, builtins.GetBuiltin(name.LiteralRepr()))
		builtin := builtins.GetBuiltin(name.LiteralRepr())
		call.ReturnType = builtin.Returns
		return
	} else {
		tc.TypeCheck(call.Callable)
	}

	switch actual := call.Callable.Type().(type) {
	case *ast.Proto_Function:
		// check the arguments

		if len(call.Arguments) != len(actual.Params.InternalTypes) {
			var msg strings.Builder
			line := call.Start.TokenSpan.Line
			col := call.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
				actual.TypeSignature(), len(actual.Params.InternalTypes), len(call.Arguments)))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}

		for index, arg := range call.Arguments {
			tc.TypeCheck(arg)
			param := actual.Params.InternalTypes[index]

			if arg.Type().TypeSignature() != param.TypeSignature() {
				var msg strings.Builder
				line := call.Start.TokenSpan.Line
				col := call.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("Expected argument %d to be typed %s but got argument of type %s.",
					index+1, param.TypeSignature(), arg.Type().TypeSignature()))
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
		}
		call.ReturnType = actual.Return
	default:
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s type cannot be called.", actual.TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}
}

func (tc *TypeChecker) VerifyType(ptype ast.ProtoType, span lexer.Span) {
	switch ac := ptype.(type) {
	case *ast.Proto_UserDef:
		found_type := tc.GetTypeForName(ac.Name.Token)
		if found_type == nil {
			var msg strings.Builder
			line := span.Line
			col := span.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Type '%s' is not defined.", ac.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
		}
	case *ast.Proto_Array:
		tc.VerifyType(ac.InternalType, span)
	case *ast.Proto_Tuple:
		for _, inner := range ac.InternalTypes {
			tc.VerifyType(inner, span)
		}
	default:
		return
	}
}

func (tc *TypeChecker) TypeCheckFunctionDef(fn *ast.FunctionDef) {
	tc.SetTypeForName(fn.Name.Token, fn.FunctionTypeSignature)
	tc.EnterTypeEnv()
	for _, param := range fn.ParameterList {
		tc.VerifyType(param.Id_Type, param.Token.TokenSpan)
		tc.SetTypeForName(param.Token, param.Id_Type)
	}
	prevFnDefSpan := tc.FnDefSpan
	prevCurReturnType := tc.CurReturnType
	tc.FnDefSpan = &fn.Start.TokenSpan

	tc.VerifyType(fn.ReturnType, fn.Body.Start.TokenSpan)
	tc.CurReturnType = fn.ReturnType
	prev := tc.CurBlockType
	tc.CurBlockType = FUNCTION
	tc.TypeCheckBlockExpr(fn.Body, false)
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
			fetched := tc.GetTypeForNameOrFail(actual.Name.Token).(*ast.Proto_UserDef)
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
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
			}
		default:
			var msg strings.Builder
			line := mem.Start.TokenSpan.Line
			col := mem.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Expected an identifier to access member of Struct but got %s, which is of type %s.",
				mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
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
				msg.WriteString(fmt.Sprintf("Tuple has %d item (indexed from 0 to %d), so provided index (%d) is out of range.",
					tup_len, tup_len-1, num))
				shared.ReportErrorAndExit("TypeChecker", msg.String())
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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
		}
	case *ast.Proto_Reference:
		switch actual := actual.Inner.(type) {
		case *ast.Proto_UserDef:
			switch ref := member.(type) {
			case *ast.Identifier:
				found := false
				fetched := tc.GetTypeForNameOrFail(actual.Name.Token).(*ast.Proto_UserDef)
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
					shared.ReportErrorAndExit("TypeChecker", msg.String())
					tc.FoundError = true
				}
			default:
				var msg strings.Builder
				line := mem.Start.TokenSpan.Line
				col := mem.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("Expected an identifier to access member of Struct but got %s, which is of type %s.",
					mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
			}
		default:
			var msg strings.Builder
			line := mem.Start.TokenSpan.Line
			col := mem.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Expected a reference to a Struct type as target for membership operation but got %s, which is of type %s.",
				mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
		}
	default:
		var msg strings.Builder
		line := mem.Start.TokenSpan.Line
		col := mem.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Expected a Struct type as target for membership operation but got %s, which is of type %s.",
			mem.Object.LiteralRepr(), mem.Object.Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckStructInit(i *ast.StructInitialization) {
	actual := tc.GetTypeForNameOrFail(i.StructName.Token)
	// actual := i.Type()

	switch sdef := actual.(type) {
	case *ast.Proto_UserDef:
		def := sdef.Definition
		i.StructType.Definition = def
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
						shared.ReportErrorAndExit("TypeChecker", msg.String())
						tc.FoundError = true
					}

				}
			}
		}
	}
}

func (tc *TypeChecker) TypeCheckStruct(s *ast.Struct) {
	tc.SetTypeForName(s.Name.Token, &ast.Proto_UserDef{
		Name:       &s.Name,
		Definition: s,
	})

	for _, mem := range s.Members {
		switch actual := mem.Id_Type.(type) {
		case *ast.Proto_UserDef:
			mem.Id_Type = tc.GetTypeForNameOrFail(actual.Name.Token)
		case *ast.Proto_Reference:
			switch inner := actual.Inner.(type) {
			case *ast.Proto_UserDef:
				actual.Inner = tc.GetTypeForNameOrFail(inner.Name.Token)
			}
		case *ast.Proto_Tuple:
			for index, _type := range actual.InternalTypes {
				if user_def, ok := _type.(*ast.Proto_UserDef); ok {
					actual.InternalTypes[index] = tc.GetTypeForNameOrFail(user_def.Name.Token)
				}
			}
		case *ast.Proto_Array:
			if user_def, ok := actual.InternalType.(*ast.Proto_UserDef); ok {
				actual.InternalType = tc.GetTypeForNameOrFail(user_def.Name.Token)
			}
		}
	}
}

func (tc *TypeChecker) TypeCheckAssignment(assign *ast.Assignment) {
	assigned := assign.Assigned
	target := assign.Target
	tc.TypeCheck(assigned)

	switch actual := target.(type) {
	case *ast.Identifier:
		if actual.Token.Literal == "_" && assign.AssignmentToken.Literal == "=" {
			return
		} else {
			tc.TypeCheck(target)
		}
	default:
		tc.TypeCheck(target)
	}

	switch assign.AssignmentToken.Literal {
	case lexer.ASSIGN:
		switch actual := target.Type().(type) {
		case *ast.Proto_Reference:
			if actual.Inner.TypeSignature() != assigned.Type().TypeSignature() &&
				actual.TypeSignature() != assigned.Type().TypeSignature() {
				var msg strings.Builder
				line := assign.AssignmentToken.TokenSpan.Line
				col := assign.AssignmentToken.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(fmt.Sprintf("The target of the assignment %s is typed %s but was assigned value of type %s.",
					target.LiteralRepr(), target.Type().TypeSignature(), assigned.Type().TypeSignature()))
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
			return
		}
		if target.Type().TypeSignature() != assigned.Type().TypeSignature() {
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The target of the assignment %s is typed %s but was assigned value of type %s.",
				target.LiteralRepr(), target.Type().TypeSignature(), assigned.Type().TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	case lexer.PLUS_EQUAL, lexer.MINUS_EQUAL, lexer.STAR_EQUAL,
		lexer.SLASH_EQUAL, lexer.MODULO_EQUAL:
		ops := map[string]string{
			"+=": "+",
			"-=": "-",
			"*=": "*",
			"/=": "/",
			"%=": "%",
		}
		found := false
		for _, bin_op := range Builtin_binary_ops {
			mapped_op := ops[assign.AssignmentToken.Literal]
			if _, allows := bin_op.AllowsBinding(mapped_op, target.Type(), assigned.Type()); allows {
				found = true
			}
		}

		if !found {
			var msg strings.Builder
			line := assign.AssignmentToken.TokenSpan.Line
			col := assign.AssignmentToken.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("The assigned value %s is typed %s and cannot use the %s operator with a target of type %s.",
				assigned.LiteralRepr(), assigned.Type().TypeSignature(), assign.AssignmentToken.Literal, target.Type().TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
		// for some reason, it hold on to the struct without its
		// necessary contents so I have to do this to provide
		// the necessary items
		index.ValueType = actual.InternalType
		switch a := actual.InternalType.(type) {
		case *ast.Proto_UserDef:
			val := tc.GetTypeForNameOrFail(a.Name.Token)
			index.ValueType = val
		}
	case *ast.Proto_Tuple:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Indexing a Tuple is not allowed. Use membership indexing instead (e.g tuple.0).")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	case *ast.Proto_Range:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Indexing a Range is not allowed.")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	case *ast.Proto_Builtin:
		if actual.TypeSignature() == "str" {
			tc.TypeCheck(index.Index)
			if index.Index.Type().TypeSignature() != "i64" {
				// can only index with i64
				var msg strings.Builder
				line := index.Start.TokenSpan.Line
				col := index.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("Arrays take index of type i64, got " + index.Indexable.Type().TypeSignature() + " instead.")
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
			index.ValueType = &ast.Proto_Builtin{
				TypeToken: lexer.ProtoToken{
					Type:      lexer.CHAR_TYPE,
					Literal:   "char",
					TokenSpan: lexer.Span{},
				},
			}
		} else {
			var msg strings.Builder
			line := index.Start.TokenSpan.Line
			col := index.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Indexing is not allowed for the %s type.", actual.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	default:
		var msg strings.Builder
		line := index.Start.TokenSpan.Line
		col := index.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Indexing is not allowed for the %s type.", actual.TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	if ir.Start.Type().TypeSignature() == "char" && ir.Start.LiteralRepr() == "''" {
		var msg strings.Builder
		line := ir.Operator.TokenSpan.Line
		col := ir.Operator.TokenSpan.Col
		start := ir.Start.LiteralRepr()
		past_end := ir.End.LiteralRepr()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Expected non-empty character, but got " + start + ".." + past_end + ".")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	ir.RangeType.InternalType = ir.Start.Type()
	ir.RangeType.IsInclusiveRange = true
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	if r.Start.Type().TypeSignature() == "char" && r.Start.LiteralRepr() == "''" {
		var msg strings.Builder
		line := r.Operator.TokenSpan.Line
		col := r.Operator.TokenSpan.Col
		start := r.Start.LiteralRepr()
		past_end := r.PastEnd.LiteralRepr()
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Expected non-empty character, but got " + start + ".." + past_end + ".")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
		return
	}

	r.RangeType.InternalType = r.Start.Type()
	r.RangeType.IsInclusiveRange = false
}

func (tc *TypeChecker) TypeCheckCollectionsFor(col_for *ast.CollectionsForLoop) {
	tc.TypeCheck(col_for.Collection)

	switch actual := col_for.Collection.Type().(type) {
	case *ast.Proto_Array:
		tc.EnterTypeEnv()
		tc.SetTypeForName(col_for.LoopVar.Token, actual.InternalType)
		prev := tc.CurBlockType
		tc.CurBlockType = LOOP
		tc.TypeCheckBlockStmt(col_for.Body, false)
		tc.CurBlockType = prev
		tc.ExitTypeEnv()
	case *ast.Proto_Builtin:
		if actual.TypeSignature() == "str" {
			tc.EnterTypeEnv()
			tc.SetTypeForName(col_for.LoopVar.Token, actual)
			prev := tc.CurBlockType
			tc.CurBlockType = LOOP
			tc.TypeCheckBlockStmt(col_for.Body, false)
			tc.CurBlockType = prev
			tc.ExitTypeEnv()
		} else {
			var msg strings.Builder
			line := col_for.Start.TokenSpan.Line
			col := col_for.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Collections looping through a '%s' is not allowed.", actual.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
			return
		}
	case *ast.Proto_Range:
		tc.EnterTypeEnv()
		tc.SetTypeForName(col_for.LoopVar.Token, actual.InternalType)
		prev := tc.CurBlockType
		tc.CurBlockType = LOOP
		tc.TypeCheckBlockStmt(col_for.Body, false)
		tc.CurBlockType = prev
		tc.ExitTypeEnv()
	case *ast.Proto_Tuple:
		var msg strings.Builder
		line := col_for.Start.TokenSpan.Line
		col := col_for.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Collections looping through a Tuple is not allowed ")
		msg.WriteString("as types may not be consistent. Try an array if types are consistent.")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
	tc.TypeCheck(loop.Update)
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlockStmt(loop.Body, false)
	tc.CurBlockType = prev
	tc.ExitTypeEnv()
}

func (tc *TypeChecker) TypeCheckInfiniteLoop(loop *ast.InfiniteLoop) {
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlockStmt(loop.Body, true)
	tc.CurBlockType = prev
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
	prev := tc.CurBlockType
	tc.CurBlockType = LOOP
	tc.TypeCheckBlockStmt(loop.Body, true)
	tc.CurBlockType = prev
}

func (tc *TypeChecker) TypeCheckIfExpr(cond *ast.IfExpr) {
	tc.TypeCheck(cond.Condition)

	if cond.Condition.Type().TypeSignature() != "bool" {
		var msg strings.Builder
		line := cond.Start.TokenSpan.Line
		col := cond.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Condition for If expression should be of type boolean")
		msg.WriteString(" but got " + cond.Condition.Type().TypeSignature() + ".")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}

	tc.EnterTypeEnv()
	prev := tc.CurBlockType
	tc.CurBlockType = IF_EXPR
	tc.TypeCheckBlockExpr(cond.ThenBody, false)
	tc.CurBlockType = prev
	tc.ExitTypeEnv()

	if cond.ElseBody != nil {
		prev := tc.CurBlockType
		tc.CurBlockType = IF_EXPR
		tc.TypeCheck(cond.ElseBody)
		tc.CurBlockType = prev

		if expr, ok := cond.ElseBody.(ast.Expression); ok {
			if cond.ThenBody.Type().TypeSignature() != expr.Type().TypeSignature() {
				// unexpected return type
				var msg strings.Builder
				line := cond.Start.TokenSpan.Line
				col := cond.Start.TokenSpan.Col
				then := cond.ThenBody.BlockType.TypeSignature()
				else_ := expr.Type().TypeSignature()
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("Then expression block is typed " + then + " and Else block is typed " + else_ + ". ")
				msg.WriteString("Please fix inconsistent types.")
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
		} else {
			if cond.ThenBody.Type().TypeSignature() != "()" {
				// unexpected return type
				var msg strings.Builder
				line := cond.Start.TokenSpan.Line
				col := cond.Start.TokenSpan.Col
				then := cond.ThenBody.BlockType.TypeSignature()
				else_ := "()"
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("If block is typed " + then + " and Else block is typed " + else_ + ". ")
				msg.WriteString("Please fix inconsistent types.")
				shared.ReportErrorAndExit("TypeChecker", msg.String())
				tc.FoundError = true
				return
			}
		}
	}
	cond.IfType = cond.ThenBody.BlockType
}

func (tc *TypeChecker) TypeCheckIfStmt(cond *ast.IfStmt) {
	tc.TypeCheck(cond.Condition)

	if cond.Condition.Type().TypeSignature() != "bool" {
		var msg strings.Builder
		line := cond.Start.TokenSpan.Line
		col := cond.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Condition for If expression should be of type boolean")
		msg.WriteString(" but got " + cond.Condition.Type().TypeSignature() + ".")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}

	tc.EnterTypeEnv()
	prev := tc.CurBlockType
	tc.CurBlockType = IF_EXPR
	tc.TypeCheckBlockStmt(cond.ThenBody, false)
	tc.CurBlockType = prev
	tc.ExitTypeEnv()

	if cond.ElseBody != nil {
		prev := tc.CurBlockType
		tc.CurBlockType = IF_EXPR
		tc.TypeCheck(cond.ElseBody)
		tc.CurBlockType = prev
	}
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
				shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
				shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
}

func (tc *TypeChecker) TypeCheckBlockExpr(block *ast.BlockExpr, new_env bool) {
	var block_type ast.ProtoType = &ast.Proto_Unit{}
	if new_env {
		tc.EnterTypeEnv()
	}

	is_return := false
	for index, node := range block.Contents {
		prev := tc.CurBlockType
		switch node.(type) {
		case *ast.BlockStmt:
			tc.CurBlockType = NONE
		}
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
						shared.ReportErrorAndExit("TypeChecker", msg.String())
						tc.FoundError = true
					}
				}
				block_type = actual.Type()
			case *ast.Return:
				is_return = true
				// do nothing, since it has been checked
			default:
				if tc.CurReturnType != nil {
					// we are in a function
					if tc.CurBlockType == FUNCTION && tc.CurReturnType.TypeSignature() != "()" {
						var msg strings.Builder
						line := tc.FnDefSpan.Line
						col := tc.FnDefSpan.Col
						msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
						msg.WriteString(fmt.Sprintf("implicit return type () does not match function return type, which is %s.",
							tc.CurReturnType.TypeSignature()))
						shared.ReportErrorAndExit("TypeChecker", msg.String())
						tc.FoundError = true
					}
				}
				block_type = &ast.Proto_Unit{}
			}
		}
		tc.CurBlockType = prev
	}
	if tc.CurBlockType == FUNCTION && !is_return && block_type.TypeSignature() != tc.CurReturnType.TypeSignature() {
		var msg strings.Builder
		line := tc.FnDefSpan.Line
		col := tc.FnDefSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("implicit return type %s does not match function return type, which is %s.",
			block_type.TypeSignature(), tc.CurReturnType.TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	}
	block.BlockType = block_type
	if new_env {
		tc.ExitTypeEnv()
	}
}

func (tc *TypeChecker) TypeCheckBlockStmt(block *ast.BlockStmt, new_env bool) {
	if new_env {
		tc.EnterTypeEnv()
	}

	for _, node := range block.Contents {
		prev := tc.CurBlockType
		switch node.(type) {
		case *ast.BlockStmt:
			tc.CurBlockType = NONE
		}
		tc.TypeCheck(node)
		tc.CurBlockType = prev
	}

	if tc.CurReturnType != nil {
		// we are in a function
		if tc.CurBlockType == FUNCTION && tc.CurReturnType.TypeSignature() != "()" {
			var msg strings.Builder
			line := tc.FnDefSpan.Line
			col := tc.FnDefSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("implicit return type () does not match function return type, which is %s.",
				tc.CurReturnType.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			tc.FoundError = true
		}
	}

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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
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
			shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		msg.WriteString("Undefined identifier '" + ident.LiteralRepr() + "'")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		tc.FoundError = true
	} else {
		ident.Id_Type = tc.GetTypeForNameOrFail(ident.Token)
	}
}

func (tc *TypeChecker) TypeCheckVariableDecl(var_def *ast.VariableDecl) {
	if var_def.Assigned == nil {
		tc.SetTypeForName(var_def.Assignee.Token, var_def.VarType)
		return
	}

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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
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
		shared.ReportErrorAndExit("TypeChecker", msg.String())
	}

	tc.SetTypeForName(var_def.Assignee.Token, var_def.VarType)
}
