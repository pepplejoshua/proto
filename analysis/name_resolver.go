package analysis

import (
	"fmt"
	"proto/ast"
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

type NameResolver struct {
	Scopes     []map[string]bool
	ScopeTag   EnclosingScopeTag
	LoopTag    EnclosingScopeTag
	FoundError bool
}

func NewNameResolver() *NameResolver {
	return &NameResolver{
		Scopes:     []map[string]bool{},
		ScopeTag:   None,
		LoopTag:    None,
		FoundError: false,
	}
}

func (nr *NameResolver) EnterScope() {
	newScope := make(map[string]bool)
	nr.Scopes = append(nr.Scopes, newScope)
}

func (nr *NameResolver) ExitScope() {
	nr.Scopes = nr.Scopes[:len(nr.Scopes)-1]
}

func (nr *NameResolver) topscope() map[string]bool {
	return nr.Scopes[len(nr.Scopes)-1]
}

func (nr *NameResolver) DeclareName(name lexer.ProtoToken) {
	if len(nr.Scopes) == 0 {
		return
	}

	scope := nr.topscope()
	scope[name.Literal] = false
}

func (nr *NameResolver) GetBoolAtName(name lexer.ProtoToken) bool {
	for i := len(nr.Scopes) - 1; i >= 0; i-- {
		cur_scope := nr.Scopes[i]
		if val, ok := cur_scope[name.Literal]; ok {
			if !val {
				continue
			} else {
				return val
			}
		}
	}
	return false
}

func (nr *NameResolver) DefineName(name lexer.ProtoToken) {
	if len(nr.Scopes) == 0 {
		return
	}
	nr.topscope()[name.Literal] = true
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
	for _, fn := range prog.FunctionDefs {
		nr.DeclareName(fn.Name.Token)
		nr.DefineName(fn.Name.Token)
	}

	for _, struct_node := range prog.Structs {
		nr.DeclareName(struct_node.Name.Token)
		nr.DefineName(struct_node.Name.Token)
	}

	for _, node := range prog.Contents {
		nr.Resolve(node)
	}
	nr.ExitScope()
}

func (nr *NameResolver) Resolve(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.Block:
		nr.ResolveBlockExpr(actual)
	case *ast.VariableDecl:
		nr.ResolveVariableDecl(actual)
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
	case *ast.IfConditional:
		nr.ResolveIfConditional(actual)
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
		nr.ResolveInfiniteLoop(actual)
		nr.LoopTag = enclosing
	case *ast.Return:
		if nr.ScopeTag != RegularFunction {
			// can't allow return statement outside function
			var msg strings.Builder
			line := actual.Token.TokenSpan.Line
			col := actual.Token.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString("a return statement is not allowed outside of a Function.")
			shared.ReportError("NameResolver", msg.String())
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
			shared.ReportError("NameResolver", msg.String())
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
			shared.ReportError("NameResolver", msg.String())
			nr.FoundError = true
		}
	// case *ast.Membership:
	// 	nr.ResolveMembership(actual)
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit, *ast.Struct:
		// do nothing
	default:
		shared.ReportError("NameResolver", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
	}
}

func (nr *NameResolver) ResolveStructInit(init *ast.StructInitialization) {
	nr.Resolve(init.StructName)
	for _, expr := range init.Fields {
		nr.Resolve(expr)
	}
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

func (nr *NameResolver) ResolveInfiniteLoop(inf *ast.InfiniteLoop) {
	nr.ResolveBlockExpr(inf.Body)
}

func (nr *NameResolver) ResolveCollectionsForLoop(cfor *ast.CollectionsForLoop) {
	nr.EnterScope()
	nr.DeclareName(cfor.LoopVar.Token)
	nr.Resolve(cfor.Collection)
	nr.DefineName(cfor.LoopVar.Token)
	nr.ResolveBlockExpr(cfor.Body)
	nr.ExitScope()
}

func (nr *NameResolver) ResolveGenericForLoop(gfor *ast.GenericForLoop) {
	nr.EnterScope()
	nr.ResolveVariableDecl(gfor.Init)
	nr.Resolve(gfor.LoopCondition)
	nr.Resolve(gfor.Update)
	nr.ResolveBlockExpr(gfor.Body)
	nr.ExitScope()
}

func (nr *NameResolver) ResolveWhileLoop(while *ast.WhileLoop) {
	nr.Resolve(while.LoopCondition)
	nr.ResolveBlockExpr(while.Body)
}

func (nr *NameResolver) ResolveIfConditional(if_ *ast.IfConditional) {
	nr.Resolve(if_.Condition)
	nr.Resolve(if_.ThenBody)

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
		nr.DeclareName(fn.Name.Token)
		nr.DefineName(fn.Name.Token)
	}

	nr.EnterScope()
	for _, param := range fn.ParameterList {
		nr.DeclareName(param.Token)
		nr.DefineName(param.Token)
	}
	nr.ResolveBlockExpr(fn.Body)
	nr.ExitScope()
	nr.ScopeTag = enclosing
}

func (nr *NameResolver) ResolveTuple(tuple *ast.Tuple) {
	for _, item := range tuple.Items {
		nr.Resolve(item)
	}
}

func (nr *NameResolver) ResolveArray(array *ast.Array) {
	for _, item := range array.Items {
		nr.Resolve(item)
	}
}

func (nr *NameResolver) ResolveAssignment(assignment *ast.Assignment) {
	nr.Resolve(assignment.Assigned)
	nr.Resolve(assignment.Target)
}

func (nr *NameResolver) ResolveBlockExpr(block *ast.Block) {
	nr.EnterScope()

	for _, node := range block.Contents {
		nr.Resolve(node)
	}
	nr.ExitScope()
}

func (nr *NameResolver) ResolveVariableDecl(var_def *ast.VariableDecl) {
	nr.DeclareName(var_def.Assignee.Token)
	if var_def.Assigned != nil {
		nr.Resolve(var_def.Assigned)
	}
	nr.DefineName(var_def.Assignee.Token)
}

func (nr *NameResolver) ResolveIdentifier(ident *ast.Identifier) {
	if !nr.ContainsIdent(ident.LiteralRepr()) || !nr.GetBoolAtName(ident.Token) {
		var msg strings.Builder
		line := ident.Token.TokenSpan.Line
		col := ident.Token.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("Undefined variable '" + ident.LiteralRepr() + "'")
		shared.ReportError("NameResolver", msg.String())
		nr.FoundError = true
	}
}
