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
			Id_Type: "",
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
		p.consume(p.cur.Type)
		val = p.parse_expr()
		p.consume(lexer.CLOSE_PAREN)
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an i64|bool|str|char|identifier|grouped expression but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return val
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
			Op_Type:  "",
		}
	default:
		val = p.parse_primary()
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
			Op_Type:  "",
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
			Op_Type:  "",
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
			Op_Type:  "",
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
			Op_Type:  "",
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
			Op_Type:  "",
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
			Op_Type:  "",
		}
	}
	return or
}

func (p *Parser) parse_expr() ast.Expression {
	return p.parse_or()
}

// func (p *Parser) parse_type()

// func (p *Parser) parse_let_mut() *ast.VariableDecl {
// 	p.consume(p.cur.Type)

// 	ident := p.parse_identifier()
// 	var var_type string

// 	if p.cur.Type == lexer.COLON {
// 		p.consume(p.cur.Type)
// 		// handle parsing type
// 		// var_type =
// 	}

// }

func Parse(src string) *ast.ProtoProgram {
	parser := New(src)
	prog := top_level(parser)
	return prog
}

func top_level(p *Parser) *ast.ProtoProgram {
	code := &ast.ProtoProgram{
		Contents: []ast.ProtoNode{},
	}

	for p.cur.Type != lexer.END {
		var node ast.ProtoNode
		switch p.cur.Type {
		// case lexer.LET, lexer.MUT:
		// 	node = p.parse_let_mut()
		default:
			node = p.parse_expr()
			if p.cur.Type == lexer.SEMI_COLON { // allow conditional semi-colon
				p.consume(p.cur.Type)
			}
		}
		code.Contents = append(code.Contents, node)
	}

	return code
}
