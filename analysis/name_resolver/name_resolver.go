package name_resolver

import (
	"fmt"
	"proto/ast"
	"proto/builtins"
	"proto/lexer"
	"proto/shared"
	"strings"
)

type EnclosingScopeTag byte

const (
	None EnclosingScopeTag = iota
	RegularFunction
	Loop
)

type Pair struct {
	Value       ast.ProtoNode
	Mutable     bool
	Initialized bool
	Defined     bool
}

type NameResolver struct {
	Scopes     []map[string]*Pair
	ScopeTag   EnclosingScopeTag
	LoopTag    EnclosingScopeTag
	ImportInfo map[string]ast.ProtoNode
	FoundError bool
}

func NewNameResolver() *NameResolver {
	return &NameResolver{
		Scopes:     []map[string]*Pair{},
		ScopeTag:   None,
		LoopTag:    None,
		ImportInfo: map[string]ast.ProtoNode{},
		FoundError: false,
	}
}

func NewNameResolverWithImportInfo(info map[string]ast.ProtoNode) *NameResolver {
	return &NameResolver{
		Scopes:     []map[string]*Pair{},
		ScopeTag:   None,
		LoopTag:    None,
		ImportInfo: info,
		FoundError: false,
	}
}

func (nr *NameResolver) EnterScope() {
	newScope := make(map[string]*Pair)
	nr.Scopes = append(nr.Scopes, newScope)
}

func (nr *NameResolver) ExitScope() {
	nr.Scopes = nr.Scopes[:len(nr.Scopes)-1]
}

func (nr *NameResolver) topscope() map[string]*Pair {
	return nr.Scopes[len(nr.Scopes)-1]
}

func (nr *NameResolver) DeclareName(name lexer.ProtoToken, value ast.ProtoNode, mutable bool) {
	if len(nr.Scopes) == 0 {
		return
	}

	scope := nr.topscope()
	scope[name.Literal] = &Pair{
		Value:       value,
		Mutable:     mutable,
		Initialized: false,
		Defined:     false,
	}
}

func (nr *NameResolver) DefineName(name lexer.ProtoToken) {
	if len(nr.Scopes) == 0 {
		return
	}

	if pair, ok := nr.topscope()[name.Literal]; ok {
		pair.Defined = true
	} else {
		var msg strings.Builder
		line := name.TokenSpan.Line
		col := name.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Undeclared variable '" + name.Literal + "'.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	}
}

func (nr *NameResolver) InitializeName(name lexer.ProtoToken) {
	if len(nr.Scopes) == 0 {
		return
	}

	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			val.Initialized = true
			return
		}
	}

	var msg strings.Builder
	line := name.TokenSpan.Line
	col := name.TokenSpan.Col
	msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
	msg.WriteString("Undeclared variable '" + name.Literal + "'.")
	shared.ReportErrorAndExit("NameResolver", msg.String())
	nr.FoundError = true
}

func (nr *NameResolver) GetDefinedAtName(name lexer.ProtoToken) bool {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			if val.Defined {
				return true
			}
		}
	}
	return false
}

func (nr *NameResolver) GetInitializedAtName(name lexer.ProtoToken) bool {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			if val.Initialized {
				return true
			}
		}
	}
	return false
}

func (nr *NameResolver) GetValueAtName(name lexer.ProtoToken) ast.ProtoNode {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			return val.Value
		}
	}
	return nil
}

func (nr *NameResolver) GetMutabilityAtName(name lexer.ProtoToken) bool {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			return val.Mutable
		}
	}
	var msg strings.Builder
	line := name.TokenSpan.Line
	col := name.TokenSpan.Col
	msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
	msg.WriteString("Undeclared variable '" + name.Literal + "'.")
	shared.ReportErrorAndExit("NameResolver", msg.String())
	nr.FoundError = true
	return false
}

func (nr *NameResolver) ContainsIdent(ident string) bool {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if _, ok := cur_scope[ident]; ok {
			return ok
		}
	}
	return false
}

func (nr *NameResolver) ResolveProgram(prog *ast.ProtoProgram) {
	nr.EnterScope()
	for _, builtin := range builtins.Builtins {
		b_token := lexer.ProtoToken{
			Type:      lexer.IDENT,
			Literal:   builtin.Name,
			TokenSpan: lexer.Span{},
		}
		nr.DeclareName(b_token, builtin, false)
		nr.DefineName(b_token)
		nr.InitializeName(b_token)
	}

	for _, fn := range prog.FunctionDefs {
		nr.DeclareName(fn.Name.Token, fn, false)
		nr.DefineName(fn.Name.Token)
		nr.InitializeName(fn.Name.Token)
	}

	for _, struct_node := range prog.Structs {
		nr.DeclareName(struct_node.Name.Token, struct_node, false)
		nr.DefineName(struct_node.Name.Token)
		nr.InitializeName(struct_node.Name.Token)
	}

	for _, node := range prog.Contents {
		nr.Resolve(node)
	}
	nr.ExitScope()
}

func (nr *NameResolver) Resolve(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.BlockExpr:
		nr.ResolveBlockExpr(actual, false)
	case *ast.BlockStmt:
		nr.ResolveBlockStmt(actual, true)
	case *ast.VariableDecl:
		nr.ResolveVariableDecl(actual)
	case *ast.Dereference:
		nr.Resolve(actual.Value)
	case *ast.Identifier:
		nr.ResolveIdentifier(actual)
	case *ast.Assignment:
		nr.ResolveAssignment(actual)
	case *ast.PromotedExpr:
		nr.Resolve(actual.Expr)
	case *ast.Tuple:
		nr.ResolveTuple(actual)
	case *ast.Array:
		nr.ResolveArray(actual)
	case *ast.FunctionDef:
		nr.ResolveFunctionDef(actual, RegularFunction)
	case *ast.CallExpression:
		nr.ResolveCallExpr(actual)
	case *ast.IfExpr:
		nr.ResolveIfExpr(actual)
	case *ast.IfStmt:
		nr.ResolveIfStmt(actual)
	case *ast.BinaryOp:
		nr.Resolve(actual.Left)
		nr.Resolve(actual.Right)
	case *ast.UnaryOp:
		nr.Resolve(actual.Operand)
	case *ast.WhileLoop:
		enclosing := nr.LoopTag
		nr.LoopTag = Loop
		nr.ResolveWhileLoop(actual)
		nr.LoopTag = enclosing
	case *ast.GenericForLoop:
		enclosing := nr.LoopTag
		nr.LoopTag = Loop
		nr.ResolveGenericForLoop(actual)
		nr.LoopTag = enclosing
	case *ast.CollectionsForLoop:
		enclosing := nr.LoopTag
		nr.LoopTag = Loop
		nr.ResolveCollectionsForLoop(actual)
		nr.LoopTag = enclosing
	case *ast.InfiniteLoop:
		enclosing := nr.LoopTag
		nr.LoopTag = Loop
		nr.ResolveBlockStmt(actual.Body, true)
		nr.LoopTag = enclosing
	case *ast.Reference:
		nr.Resolve(actual.Value)
	case *ast.Return:
		if nr.ScopeTag != RegularFunction {
			// can't allow return statement outside function
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("a return statement is not allowed outside of a Function.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
		if actual.Value != nil {
			nr.Resolve(actual.Value)
		}
	case *ast.InclusiveRange:
		nr.ResolveInclusiveRange(actual)
	case *ast.Range:
		nr.ResolveRange(actual)
	case *ast.IndexExpression:
		nr.ResolveIndexExpr(actual)
	case *ast.Struct:
		nr.ResolveStruct(actual)
	case *ast.StructInitialization:
		nr.ResolveStructInit(actual)
	case *ast.Break:
		if nr.LoopTag != Loop {
			// can't allow break or continue statement outside loop
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("a break statement is not allowed outside of a Loop.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
	case *ast.Continue:
		if nr.LoopTag != Loop {
			// can't allow break or continue statement outside loop
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("a continue statement is not allowed outside of a Loop.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
	case *ast.Membership:
		nr.ResolveMembership(actual)
	case *ast.Module:
		nr.ResolveModule(actual)
	case *ast.UseStmt:
		nr.ResolveUseStmt(actual)
	case *ast.ModuleAccess:
		nr.ResolveModuleAccess(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit:
		// do nothing
	default:
		shared.ReportErrorAndExit("NameResolver", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
	}
}

func (nr *NameResolver) ResolveModuleAccess(access *ast.ModuleAccess) {
	nr.Resolve(access.Mod)
}

func (nr *NameResolver) ResolveUseStmt(use *ast.UseStmt) {
	// need to generate permanent info to be used by all passes
	// so name resolver can have all symbols resolved in imported files
	// here and type checker can verify types of symbols from imported files
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
			mod := nr.ImportInfo[tag+name].(*ast.Module)

			for _, m := range mod.Body.Modules {
				nr.DeclareName(m.Name.Token, m, false)
				nr.DefineName(m.Name.Token)
				nr.InitializeName(m.Name.Token)
			}

			for _, fn := range mod.Body.Functions {
				nr.DeclareName(fn.Name.Token, fn, false)
				nr.DefineName(fn.Name.Token)
				nr.InitializeName(fn.Name.Token)
			}

			for _, decl := range mod.Body.VariableDecls {
				nr.DeclareName(decl.Assignee.Token, decl, false)
				nr.DefineName(decl.Assignee.Token)
				nr.DefineName(decl.Assignee.Token)
			}
		} else {
			if len(path.Pieces) > 1 {
				if as, ok := last_node.(*ast.UseAs); ok {
					val := nr.ImportInfo[tag+as.As.LiteralRepr()]
					nr.DeclareName(as.As.Token, val, false)
					nr.DefineName(as.As.Token)
					nr.InitializeName(as.As.Token)
				} else {
					val := nr.ImportInfo[tag+name]
					switch actual := val.(type) {
					case *ast.Module:
						nr.DeclareName(actual.Name.Token, actual, false)
						nr.DefineName(actual.Name.Token)
						nr.InitializeName(actual.Name.Token)
					case *ast.VariableDecl:
						nr.DeclareName(actual.Assignee.Token, actual, actual.Mutable)
						nr.DefineName(actual.Assignee.Token)
						nr.DefineName(actual.Assignee.Token)
					case *ast.FunctionDef:
						nr.DeclareName(actual.Name.Token, actual, false)
						nr.DefineName(actual.Name.Token)
						nr.InitializeName(actual.Name.Token)
					}
				}
			} else {
				val := nr.ImportInfo[tag+name]
				switch actual := val.(type) {
				case *ast.Module:
					nr.DeclareName(actual.Name.Token, actual, false)
					nr.DefineName(actual.Name.Token)
					nr.InitializeName(actual.Name.Token)
				case *ast.VariableDecl:
					nr.DeclareName(actual.Assignee.Token, actual, actual.Mutable)
					nr.DefineName(actual.Assignee.Token)
					nr.DefineName(actual.Assignee.Token)
				case *ast.FunctionDef:
					nr.DeclareName(actual.Name.Token, actual, false)
					nr.DefineName(actual.Name.Token)
					nr.InitializeName(actual.Name.Token)
				}
			}
		}
	}
}

func (nr *NameResolver) ResolveModule(mod *ast.Module) {
	nr.ResolveBlockStmt(mod.Body, true)
	// declare name after checking whole module
	nr.DeclareName(mod.Name.Token, mod, false)
	nr.DefineName(mod.Name.Token)
	nr.InitializeName(mod.Name.Token)
}

func (nr *NameResolver) ResolveStruct(str *ast.Struct) {
	nr.DeclareName(str.Name.Token, str, false)
	nr.DefineName(str.Name.Token)
	nr.InitializeName(str.Name.Token)
}

func (nr *NameResolver) ResolveStructInit(init *ast.StructInitialization) {
	nr.ResolveIdentifier(init.StructName)
	actual := nr.GetValueAtName(init.StructName.Token)
	switch val := actual.(type) {
	case *ast.Struct:
		found_duplicate := false
	outer:
		for id := range init.Fields {
			for check_id := range init.Fields {
				if id != check_id && id.LiteralRepr() == check_id.LiteralRepr() {
					found_duplicate = true
					break outer
				}
			}
		}

		if found_duplicate {
			var msg strings.Builder
			line := init.Start.TokenSpan.Line
			col := init.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d struct %s expected { ", line, col, val.Name.Token.Literal))
			for index, mem := range val.Members {
				msg.WriteString(mem.Token.Literal)
				msg.WriteString(": " + mem.Type().TypeSignature())

				if index+1 < len(val.Members) {
					msg.WriteString(", ")
				}
			}
			msg.WriteString(" } as members but got { ")

			length := len(init.Fields)
			index := 0
			for id, val := range init.Fields {
				msg.WriteString(id.Token.Literal)
				msg.WriteString(": " + val.LiteralRepr())
				if index+1 < length {
					msg.WriteString(", ")
				}
				index++
			}
			msg.WriteString(" }, which contains duplicates.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}

		if len(val.Members) != len(init.Fields) {
			var msg strings.Builder
			line := init.Start.TokenSpan.Line
			col := init.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d Expected %d ", line, col, len(val.Members)))
			msg.WriteString(fmt.Sprintf("members but got %d.", len(init.Fields)))
			if len(val.Members) > 1 {
				msg.WriteString("\n struct " + init.StructName.LiteralRepr() + " members include: \n")
				for index, mem := range val.Members {
					msg.WriteString(fmt.Sprintf("%d. %s: %s\n", index+1, mem.LiteralRepr(), mem.Id_Type.TypeSignature()))
				}
			}
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}

		for field, expr := range init.Fields {
			// make sure field exists in the struct
			found := false
			for _, mem := range val.Members {
				if mem.LiteralRepr() == field.LiteralRepr() {
					found = true
					break
				}
			}

			if !found {
				var msg strings.Builder
				line := field.Token.TokenSpan.Line
				col := field.Token.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString(field.LiteralRepr() + " is not a Struct member.")
				shared.ReportErrorAndExit("NameResolver", msg.String())
				nr.FoundError = true
				break
			}
			nr.Resolve(expr)
		}
		init.StructType.Definition = val

	default:
		var msg strings.Builder
		line := init.StructName.Token.TokenSpan.Line
		col := init.StructName.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(init.StructName.LiteralRepr() + " is not a Struct.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	}
}

func (nr *NameResolver) ResolveMembership(mem *ast.Membership) {
	nr.Resolve(mem.Object)
}

func (nr *NameResolver) ResolveIndexExpr(index *ast.IndexExpression) {
	nr.Resolve(index.Indexable)
	nr.Resolve(index.Index)
}

func (nr *NameResolver) ResolveInclusiveRange(inrange *ast.InclusiveRange) {
	nr.Resolve(inrange.Start)
	nr.Resolve(inrange.End)
}

func (nr *NameResolver) ResolveRange(range_ *ast.Range) {
	nr.Resolve(range_.Start)
	nr.Resolve(range_.PastEnd)
}

func (nr *NameResolver) ResolveCollectionsForLoop(cfor *ast.CollectionsForLoop) {
	nr.EnterScope()
	nr.DeclareName(cfor.LoopVar.Token, &cfor.LoopVar, false)
	nr.Resolve(cfor.Collection)
	nr.DefineName(cfor.LoopVar.Token)
	nr.InitializeName(cfor.LoopVar.Token)
	nr.ResolveBlockStmt(cfor.Body, false)
	nr.ExitScope()
}

func (nr *NameResolver) ResolveGenericForLoop(gfor *ast.GenericForLoop) {
	nr.EnterScope()
	nr.ResolveVariableDecl(gfor.Init)
	nr.Resolve(gfor.LoopCondition)
	nr.Resolve(gfor.Update)
	nr.ResolveBlockStmt(gfor.Body, false)
	nr.ExitScope()
}

func (nr *NameResolver) ResolveWhileLoop(while *ast.WhileLoop) {
	nr.Resolve(while.LoopCondition)
	nr.EnterScope()
	nr.ResolveBlockStmt(while.Body, false)
	nr.ExitScope()
}

func (nr *NameResolver) ResolveIfStmt(if_ *ast.IfStmt) {
	nr.Resolve(if_.Condition)
	nr.EnterScope()
	nr.ResolveBlockStmt(if_.ThenBody, false)
	nr.ExitScope()

	if if_.ElseBody != nil {
		nr.Resolve(if_.ElseBody)
	}
}

func (nr *NameResolver) ResolveIfExpr(if_ *ast.IfExpr) {
	nr.Resolve(if_.Condition)
	nr.EnterScope()
	nr.ResolveBlockExpr(if_.ThenBody, false)
	nr.ExitScope()

	if if_.ElseBody != nil {
		nr.Resolve(if_.ElseBody)
	}
}

func (nr *NameResolver) ResolveCallExpr(call *ast.CallExpression) {
	nr.Resolve(call.Callable)

	for _, arg := range call.Arguments {
		nr.Resolve(arg)
	}
}

func (nr *NameResolver) ResolveFunctionDef(fn *ast.FunctionDef, fnScope EnclosingScopeTag) {
	enclosing := nr.ScopeTag
	nr.ScopeTag = fnScope
	if !nr.ContainsIdent(fn.Name.LiteralRepr()) {
		nr.DeclareName(fn.Name.Token, fn, false)
		nr.DefineName(fn.Name.Token)
		nr.InitializeName(fn.Name.Token)
	}

	nr.EnterScope()
	for _, param := range fn.ParameterList {
		nr.DeclareName(param.Token, param, true)
		nr.DefineName(param.Token)
		nr.InitializeName(param.Token)
	}
	nr.ResolveBlockExpr(fn.Body, false)
	nr.ExitScope()
	nr.ScopeTag = enclosing
}

func (nr *NameResolver) ResolveTuple(tuple *ast.Tuple) {
	for _, item := range tuple.Items {
		nr.Resolve(item)
	}
}

func (nr *NameResolver) ResolveArray(array *ast.Array) {
	nr.ResolveArrayType(array.ArrayType, array.Token.TokenSpan)
	for _, item := range array.Items {
		nr.Resolve(item)
	}
}

func (nr *NameResolver) ResolveArrayType(t ast.ProtoType, span lexer.Span) {
	switch actual := t.(type) {
	case *ast.Proto_UserDef:
		val := nr.GetValueAtName(actual.Name.Token)
		switch val.(type) {
		case *ast.Struct:
		default:
			var msg strings.Builder
			line := span.Line
			col := span.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Undefined name '%s' inferred as Array type.", actual.Name.LiteralRepr()))
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
			return
		}
	case *ast.Proto_Array:
		nr.ResolveArrayType(actual.InternalType, span)
	}
}

func (nr *NameResolver) ResolveAssignment(assignment *ast.Assignment) {
	nr.Resolve(assignment.Assigned)

	switch actual := assignment.Target.(type) {
	case *ast.Identifier:
		if actual.Token.Literal == "_" && assignment.AssignmentToken.Literal == "=" {
			// do nothing
			return
		} else {
			if !nr.GetMutabilityAtName(actual.Token) && nr.GetInitializedAtName(actual.Token) {
				var msg strings.Builder
				line := actual.Token.TokenSpan.Line
				col := actual.Token.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
				msg.WriteString("'" + actual.LiteralRepr() + "' is not mutable.")
				shared.ReportErrorAndExit("NameResolver", msg.String())
				nr.FoundError = true
			}
			if !nr.GetInitializedAtName(actual.Token) {
				nr.InitializeName(actual.Token)
			}
		}
	case *ast.Dereference:
		var msg strings.Builder
		line := actual.Start.TokenSpan.Line
		col := actual.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Assigning to a dereferenced value '" + actual.LiteralRepr() + "' is not allowed as it has no effect.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	case *ast.Reference:
		var msg strings.Builder
		line := actual.Start.TokenSpan.Line
		col := actual.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Assigning to a reference value '" + actual.LiteralRepr() + "' is not allowed.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	case *ast.Membership:
		nr.CheckObjectOfMembershipForMutability(actual)
	case *ast.IndexExpression:
		nr.CheckIndexExprForMutability(actual)
	default:
		var msg strings.Builder
		line := assignment.AssignmentToken.TokenSpan.Line
		col := assignment.AssignmentToken.TokenSpan.Line
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Assigning to '" + actual.LiteralRepr() + "' is not allowed.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	}
	nr.Resolve(assignment.Target)
}

func (nr *NameResolver) CheckIndexExprForMutability(arr *ast.IndexExpression) {
	switch actual := arr.Indexable.(type) {
	case *ast.IndexExpression:
		nr.CheckIndexExprForMutability(actual)
	case *ast.Identifier:
		if !nr.GetMutabilityAtName(actual.Token) && nr.GetInitializedAtName(actual.Token) {
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("'" + actual.LiteralRepr() + "' is not mutable.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
	case *ast.Membership:
		nr.CheckObjectOfMembershipForMutability(actual)
	}
}

func (nr *NameResolver) CheckReferenceForMutability(ref *ast.Reference) {
	switch actual := ref.Value.(type) {
	case *ast.Identifier:
		if !nr.GetMutabilityAtName(actual.Token) && nr.GetInitializedAtName(actual.Token) {
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("'" + actual.LiteralRepr() + "' is not mutable.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
	case *ast.Array:
	case *ast.IndexExpression:
		nr.CheckIndexExprForMutability(actual)
	case *ast.Membership:
		nr.CheckObjectOfMembershipForMutability(actual)
	}
}

func (nr *NameResolver) CheckObjectOfMembershipForMutability(mem *ast.Membership) {
	switch actual_obj := mem.Object.(type) {
	case *ast.Identifier:
		if !nr.GetMutabilityAtName(actual_obj.Token) && nr.GetInitializedAtName(actual_obj.Token) {
			var msg strings.Builder
			line := actual_obj.Token.TokenSpan.Line
			col := actual_obj.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("'" + actual_obj.LiteralRepr() + "' is not mutable.")
			shared.ReportErrorAndExit("NameResolver", msg.String())
			nr.FoundError = true
		}
	case *ast.Membership:
		nr.CheckObjectOfMembershipForMutability(actual_obj)
	case *ast.IndexExpression:
		nr.CheckIndexExprForMutability(actual_obj)
	}
}

func (nr *NameResolver) ResolveBlockStmt(block *ast.BlockStmt, new_scope bool) {
	if new_scope {
		nr.EnterScope()
	}

	for _, node := range block.Contents {
		nr.Resolve(node)
	}

	if new_scope {
		nr.ExitScope()
	}
}

func (nr *NameResolver) ResolveBlockExpr(block *ast.BlockExpr, new_scope bool) {
	if new_scope {
		nr.EnterScope()
	}

	for _, node := range block.Contents {
		nr.Resolve(node)
	}

	if new_scope {
		nr.ExitScope()
	}
}

func (nr *NameResolver) ResolveVariableDecl(var_def *ast.VariableDecl) {
	if var_def.Assignee.Token.Literal != "_" {
		if var_def.Assigned != nil {
			nr.Resolve(var_def.Assigned)
			nr.DeclareName(var_def.Assignee.Token, &var_def.Assignee, var_def.Mutable)
			nr.DefineName(var_def.Assignee.Token)
			nr.InitializeName(var_def.Assignee.Token)
		} else {
			nr.DeclareName(var_def.Assignee.Token, &var_def.Assignee, var_def.Mutable)
			nr.DefineName(var_def.Assignee.Token)
		}
	} else {
		var msg strings.Builder
		line := var_def.Assignee.Token.TokenSpan.Line
		col := var_def.Assignee.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Declaring a variable with name '_' is not allowed.")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	}
}

func (nr *NameResolver) ResolveIdentifier(ident *ast.Identifier) {
	if !nr.ContainsIdent(ident.LiteralRepr()) || !nr.GetDefinedAtName(ident.Token) {
		var msg strings.Builder
		line := ident.Token.TokenSpan.Line
		col := ident.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Undefined name '" + ident.LiteralRepr() + "'")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	} else if !nr.GetInitializedAtName(ident.Token) {
		var msg strings.Builder
		line := ident.Token.TokenSpan.Line
		col := ident.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Use of uninitialized variable '" + ident.LiteralRepr() + "'")
		shared.ReportErrorAndExit("NameResolver", msg.String())
		nr.FoundError = true
	}
}
