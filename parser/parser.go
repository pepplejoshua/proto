package parser

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strings"
)

func New(src string) *Parser {
	l := lexer.New(src)
	p := &Parser{
		lex: l,
	}

	p.nextToken() // initialize p.peek
	p.nextToken() // initialize p.cur and update p.peek
	return p
}

type Parser struct {
	lex  *lexer.Lexer
	cur  lexer.ProtoToken
	peek lexer.ProtoToken
}

func (p *Parser) nextToken() {
	p.cur = p.peek
	p.peek = p.lex.Next_Token()
}

func (p *Parser) consume(expected lexer.TokenType) {
	if p.cur.Type == expected {
		p.nextToken() // move to next token
	} else {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected token of type ")
		msg.WriteString(string(expected) + " but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
}

func (p *Parser) parse_identifier() *ast.Identifier {
	var ident *ast.Identifier
	switch p.cur.Type {
	case lexer.IDENT:
		id := p.cur
		p.consume(id.Type)
		ident = &ast.Identifier{
			Token:   id,
			Id_Type: &ast.Proto_Untyped{},
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an identifier but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return ident
}

func (p *Parser) parse_block() *ast.Block {
	start := p.cur
	p.consume(lexer.OPEN_CURLY)
	var contents []ast.ProtoNode
	for p.cur.Type != lexer.CLOSE_CURLY {
		node := p.parse_protonode()
		contents = append(contents, node)
	}
	p.consume(lexer.CLOSE_CURLY)

	var block_type ast.ProtoType
	if len(contents) > 0 {
		last := contents[len(contents)-1]

		switch actual := last.(type) {
		case ast.Expression:
			block_type = actual.Type()
		default:
			block_type = &ast.Proto_Unit{}
		}
	} else {
		block_type = &ast.Proto_Unit{}
	}

	return &ast.Block{
		Start:     start,
		Contents:  contents,
		BlockType: block_type,
	}
}

func (p *Parser) parse_if_conditional() *ast.IfConditional {
	start := p.cur
	p.consume(p.cur.Type)

	condition := p.parse_expr()

	thenbody := p.parse_block()

	var elsebody ast.Expression = nil
	if p.cur.Type == lexer.ELSE {
		p.consume(p.cur.Type)

		if p.cur.Type == lexer.IF {
			elsebody = p.parse_if_conditional()
		} else {
			elsebody = p.parse_block()
		}
	}

	var if_type ast.ProtoType
	if (elsebody != nil &&
		thenbody.Type().TypeSignature() == elsebody.Type().TypeSignature()) ||
		elsebody == nil {
		// if we can infer 2 matching types for the blocks, then use it
		if_type = thenbody.Type()
	} else {
		if_type = &ast.Proto_Untyped{}
	}

	return &ast.IfConditional{
		Start:     start,
		Condition: condition,
		ThenBody:  thenbody,
		ElseBody:  elsebody,
		IfType:    if_type,
	}
}

func (p *Parser) parse_primary() ast.Expression {
	var val ast.Expression
	switch p.cur.Type {
	case lexer.I64:
		i64 := p.cur
		p.consume(i64.Type)
		val = &ast.I64{
			Token: i64,
		}
	case lexer.CHAR:
		char := p.cur
		p.consume(char.Type)
		val = &ast.Char{
			Token: char,
		}
	case lexer.STRING:
		str := p.cur
		p.consume(str.Type)
		val = &ast.String{
			Token: str,
		}
	case lexer.TRUE, lexer.FALSE:
		primary := p.cur
		p.consume(primary.Type)
		boolean := &ast.Boolean{
			Token: primary,
		}

		if primary.Type == lexer.TRUE {
			boolean.Value = true
		} else {
			boolean.Value = false
		}
		val = boolean
	case lexer.IDENT:
		val = p.parse_identifier()
	case lexer.OPEN_PAREN:
		start := p.cur
		p.consume(p.cur.Type)
		val = p.parse_expr()
		if p.cur.Type == lexer.COMMA { // we will parse a tuple instead
			p.consume(p.cur.Type)
			var items []ast.Expression
			var types []ast.ProtoType
			items = append(items, val)
			types = append(types, val.Type())
			for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_PAREN {
				expr := p.parse_expr()
				items = append(items, expr)
				types = append(types, expr.Type())
				if p.cur.Type != lexer.CLOSE_PAREN {
					p.consume(lexer.COMMA)
				}
			}
			p.consume(lexer.CLOSE_PAREN)
			val = &ast.Tuple{
				Items: items,
				Token: start,
				TupleType: &ast.Proto_Tuple{
					InternalTypes: types,
				},
			}
		} else {
			p.consume(lexer.CLOSE_PAREN)
		}
	case lexer.OPEN_BRACKET:
		start := p.cur
		p.consume(p.cur.Type)
		var items []ast.Expression
		var arr_type ast.ProtoType = nil
		for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_BRACKET {
			expr := p.parse_expr()
			items = append(items, expr)

			if arr_type == nil {
				// first pass
				arr_type = expr.Type()
			} else if arr_type.TypeSignature() != "untyped" &&
				expr.Type().TypeSignature() != "untyped" &&
				expr.Type().TypeSignature() != arr_type.TypeSignature() {
				// an array with inconsistent typing
				arr_type = &ast.Proto_Untyped{}
			}

			if p.cur.Type == lexer.COMMA {
				p.consume(p.cur.Type)
			}
		}
		p.consume(lexer.CLOSE_BRACKET)

		arr := &ast.Array{
			Items: items,
			Token: start,
			ArrayType: &ast.Proto_Array{
				InternalType: &ast.Proto_Untyped{},
			},
		}
		if arr_type != nil {
			arr.ArrayType.InternalType = arr_type
		}
		val = arr
	case lexer.OPEN_CURLY:
		val = p.parse_block()
	case lexer.IF:
		val = p.parse_if_conditional()
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an expression but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return val
}

func (p *Parser) parse_call_expression() ast.Expression {
	return p.parse_primary()
}

func (p *Parser) parse_unary() ast.Expression {
	var val ast.Expression
	switch p.cur.Type {
	case lexer.NOT, lexer.MINUS:
		operator := p.cur
		p.consume(operator.Type)
		operand := p.parse_unary()
		val = &ast.UnaryOp{
			Operand:  operand,
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	default:
		val = p.parse_call_expression()
	}
	return val
}

func (p *Parser) parse_factor() ast.Expression {
	factor := p.parse_unary()

	for p.cur.Type == lexer.STAR || p.cur.Type == lexer.SLASH ||
		p.cur.Type == lexer.MODULO {
		operator := p.cur
		p.consume(operator.Type)
		factor = &ast.BinaryOp{
			Left:     factor,
			Right:    p.parse_unary(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return factor
}

func (p *Parser) parse_term() ast.Expression {
	term := p.parse_factor()

	for p.cur.Type == lexer.PLUS || p.cur.Type == lexer.MINUS {
		operator := p.cur
		p.consume(operator.Type)
		term = &ast.BinaryOp{
			Left:     term,
			Right:    p.parse_factor(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return term
}

func (p *Parser) parse_comparison() ast.Expression {
	comp := p.parse_term()

	for p.cur.Type == lexer.GREATER_THAN || p.cur.Type == lexer.GREATER_OR_EQUAL ||
		p.cur.Type == lexer.LESS_THAN || p.cur.Type == lexer.LESS_OR_EQUAL {
		operator := p.cur
		p.consume(operator.Type)
		comp = &ast.BinaryOp{
			Left:     comp,
			Right:    p.parse_term(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return comp
}

func (p *Parser) parse_equality() ast.Expression {
	eq := p.parse_comparison()

	for p.cur.Type == lexer.IS_EQUAL_TO || p.cur.Type == lexer.NOT_EQUAL_TO {
		operator := p.cur
		p.consume(operator.Type)
		eq = &ast.BinaryOp{
			Left:     eq,
			Right:    p.parse_comparison(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return eq
}

func (p *Parser) parse_and() ast.Expression {
	and := p.parse_equality()

	for p.cur.Type == lexer.AND {
		operator := p.cur
		p.consume(operator.Type)
		and = &ast.BinaryOp{
			Left:     and,
			Right:    p.parse_equality(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return and
}

func (p *Parser) parse_or() ast.Expression {
	or := p.parse_and()

	for p.cur.Type == lexer.OR {
		operator := p.cur
		p.consume(operator.Type)
		or = &ast.BinaryOp{
			Left:     or,
			Right:    p.parse_and(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return or
}

func (p *Parser) parse_expr() ast.Expression {
	return p.parse_or()
}

func (p *Parser) parse_assignment(check_for_semi bool) ast.ProtoNode {
	target := p.parse_expr()

	if p.cur.Type == lexer.ASSIGN ||
		p.cur.Type == lexer.PLUS_EQUAL ||
		p.cur.Type == lexer.MINUS_EQUAL ||
		p.cur.Type == lexer.STAR_EQUAL ||
		p.cur.Type == lexer.SLASH_EQUAL ||
		p.cur.Type == lexer.MODULO_EQUAL {
		operator := p.cur
		p.consume(p.cur.Type)
		assigned := p.parse_expr()
		assignment := &ast.Assignment{
			Target:          target,
			AssignmentToken: operator,
			Assigned:        assigned,
		}

		if check_for_semi {
			p.consume(lexer.SEMI_COLON)
		}
		return assignment
	}

	return target
}

func (p *Parser) parse_type() ast.ProtoType {
	var proto_type ast.ProtoType

	switch p.cur.Type {
	case lexer.I64_TYPE, lexer.CHAR_TYPE, lexer.BOOL_TYPE, lexer.STRING_TYPE:
		proto_type = &ast.Proto_Builtin{
			TypeToken: p.cur,
		}
		p.consume(p.cur.Type)
	case lexer.OPEN_BRACKET: // we are dealing with an array
		p.consume(p.cur.Type)
		internal := p.parse_type()
		p.consume(lexer.CLOSE_BRACKET)
		proto_type = &ast.Proto_Array{
			InternalType: internal,
		}
	case lexer.OPEN_PAREN:
		p.consume(p.cur.Type)
		var contained []ast.ProtoType
		for p.cur.Type != lexer.CLOSE_PAREN && p.cur.Type != lexer.END {
			item := p.parse_type()
			contained = append(contained, item)
			if p.cur.Type == lexer.COMMA {
				p.consume(p.cur.Type)
			}
		}
		p.consume(p.cur.Type)
		proto_type = &ast.Proto_Tuple{
			InternalTypes: contained,
		}
	case lexer.IDENT:
		token := p.parse_identifier()
		proto_type = &ast.Proto_UserDef{
			Ident: *token,
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected type signature but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return proto_type
}

func (p *Parser) parse_let_mut() *ast.VariableDecl {
	keyword := p.cur.Type
	p.consume(p.cur.Type)

	ident := p.parse_identifier()

	var var_type ast.ProtoType = &ast.Proto_Untyped{}
	if p.cur.Type == lexer.COLON {
		p.consume(p.cur.Type)
		// handle parsing type
		var_type = p.parse_type()
	}

	var assigned_expr ast.Expression = nil
	if p.cur.Type == lexer.ASSIGN {
		p.consume(p.cur.Type)
		assigned_expr = p.parse_expr()
	}

	p.consume(lexer.SEMI_COLON)

	// if variable is not typed or initialized, throw an error
	if var_type.TypeSignature() == "untyped" && assigned_expr == nil {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(ident.Token.TokenSpan.Line) + ":" + fmt.Sprint(ident.Token.TokenSpan.Col))
		msg.WriteString(fmt.Sprintf(" Expected '%s' to be typed or initialized.", ident.LiteralRepr()))
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}

	if assigned_expr != nil && !strings.Contains(assigned_expr.Type().TypeSignature(), "untyped") {
		var_type = assigned_expr.Type()
	}

	declaration := &ast.VariableDecl{
		Assignee: *ident,
		Assigned: assigned_expr,
		Mutable:  false,
		VarType:  var_type,
	}

	if keyword == lexer.MUT {
		declaration.Mutable = true
	}

	return declaration
}

func (p *Parser) parse_struct() *ast.Struct {
	start := p.cur
	p.consume(p.cur.Type)

	struct_name := p.parse_identifier()
	p.consume(lexer.OPEN_CURLY)

	var members []*ast.Identifier

	for p.cur.Type == lexer.IDENT {
		mem := p.parse_identifier()
		p.consume(lexer.COLON)
		mem_type := p.parse_type()
		mem.Id_Type = mem_type
		members = append(members, mem)
	}

	p.consume(p.cur.Type)

	proto_struct := &ast.Struct{
		Start:   start,
		Name:    *struct_name,
		Members: members,
	}
	return proto_struct
}

func (p *Parser) parse_for_loop() ast.ProtoNode {
	var node ast.ProtoNode

	start := p.cur
	p.consume(p.cur.Type)

	if p.cur.Type == lexer.MUT {
		// we have a Generic for loop
		init := p.parse_let_mut()
		if init.Assigned == nil {
			// if loop variable is not initialized, throw error
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(init.Assignee.Token.TokenSpan.Line) + ":" + fmt.Sprint(init.Assignee.Token.TokenSpan.Col))
			msg.WriteString(fmt.Sprintf(" Expected '%s' to be initialized.", init.Assignee.LiteralRepr()))
			shared.ReportErrorAndExit("Parser", msg.String())
		}

		loop_condition := p.parse_expr()
		p.consume(lexer.SEMI_COLON)
		update := p.parse_assignment(false)
		body := p.parse_block()

		node = &ast.GenericForLoop{
			Start:         start,
			Init:          init,
			LoopCondition: loop_condition,
			Update:        update,
			Body:          body,
		}
	} else if p.cur.Type == lexer.IDENT && p.peek.Type == lexer.IN {
		// we have a collections for loop
		loop_var := p.parse_identifier()
		p.consume(lexer.IN)
		collection := p.parse_expr()
		body := p.parse_block()

		node = &ast.CollectionsForLoop{
			Start:      start,
			LoopVar:    *loop_var,
			Collection: collection,
			Body:       body,
		}
	} else {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Incorrectly formatted for loop.")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return node
}

func (p *Parser) parse_infinite_loop() *ast.InfiniteLoop {
	start := p.cur
	p.consume(p.cur.Type)

	body := p.parse_block()

	return &ast.InfiniteLoop{
		Start: start,
		Body:  body,
	}
}

func (p *Parser) parse_while_loop() *ast.WhileLoop {
	start := p.cur
	p.consume(p.cur.Type)

	loop_condition := p.parse_expr()
	body := p.parse_block()

	return &ast.WhileLoop{
		Start:         start,
		LoopCondition: loop_condition,
		Body:          body,
	}
}

func (p *Parser) parse_function_definition() *ast.FunctionDef {
	start := p.cur
	p.consume(p.cur.Type)

	name := p.parse_identifier()
	var paramslist []*ast.Identifier

	p.consume(lexer.OPEN_PAREN)
	for p.cur.Type != lexer.CLOSE_PAREN {
		if len(paramslist) > 255 {
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
			msg.WriteString(" Function Definitions only allows 255 parameters.")
			shared.ReportErrorAndExit("Parser", msg.String())
		}
		ident := p.parse_identifier()
		p.consume(lexer.COLON)
		ident.Id_Type = p.parse_type()
		paramslist = append(paramslist, ident)
	}
	p.consume(lexer.CLOSE_PAREN)

	var return_type ast.ProtoType = &ast.Proto_Unit{}
	if p.cur.Type == lexer.ARROW {
		p.consume(p.cur.Type)
		return_type = p.parse_type()
	}

	body := p.parse_block()

	return &ast.FunctionDef{
		Start:         start,
		Name:          name,
		ParameterList: paramslist,
		ReturnType:    return_type,
		Body:          body,
	}
}

func (p *Parser) parse_protonode() ast.ProtoNode {
	var node ast.ProtoNode
	switch p.cur.Type {
	case lexer.LET, lexer.MUT:
		node = p.parse_let_mut()
	case lexer.STRUCT:
		node = p.parse_struct()
	case lexer.FOR:
		node = p.parse_for_loop()
	case lexer.LOOP:
		node = p.parse_infinite_loop()
	case lexer.WHILE:
		node = p.parse_while_loop()
	case lexer.FN:
		node = p.parse_function_definition()
	case lexer.BREAK:
		node = &ast.Break{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		p.consume(lexer.SEMI_COLON)
	case lexer.RETURN:
		return_statement := &ast.Return{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		if p.cur.Type != lexer.SEMI_COLON {
			return_statement.Value = p.parse_expr()
		}
		p.consume(lexer.SEMI_COLON)
		node = return_statement
	case lexer.CONTINUE:
		node = &ast.Continue{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		p.consume(lexer.SEMI_COLON)
	default:
		potential_expr := p.parse_assignment(true)

		switch actual := potential_expr.(type) {
		case ast.Expression:
			if p.cur.Type == lexer.SEMI_COLON { // allow semi-colon to promote expr to statement
				p.consume(p.cur.Type)
				node = &ast.PromotedExpr{
					Expr: actual,
				}
			} else {
				node = actual
			}
		default:
			node = actual
		}
	}

	return node
}

func Parse(src string) *ast.ProtoProgram {
	parser := New(src)
	return top_level(parser)
}

func top_level(p *Parser) *ast.ProtoProgram {
	code := &ast.ProtoProgram{
		Contents: []ast.ProtoNode{},
	}

	for p.cur.Type != lexer.END {
		code.Contents = append(code.Contents, p.parse_protonode())
	}

	return code
}
