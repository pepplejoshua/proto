package ast

import (
	"proto/lexer"
	"strings"
)

type ProtoNode interface {
	LiteralRepr() string
	AsCppCode(*CodeGenerator, bool, bool)
}

// a proto program just contains a bunch of ProtoNodes
type ProtoProgram struct {
	Start         lexer.ProtoToken
	End           lexer.ProtoToken
	Path          string
	Main          *FunctionDef
	Contents      []ProtoNode
	FunctionDefs  []*FunctionDef
	Structs       []*Struct
	Imports       []*UseStmt
	Modules       []*Module
	VariableDecls []*VariableDecl
}

type CodeGenerator struct {
	final_str      string
	tab_space      string
	prev_tab_space string
	tab_count      int
	def_tab_incr   int
	includes       map[string]int
}

func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{
		final_str:      "",
		tab_space:      "",
		prev_tab_space: "",
		tab_count:      0,
		def_tab_incr:   4,
		includes:       map[string]int{},
	}
}

func (c *CodeGenerator) Indent() {
	c.prev_tab_space = c.tab_space
	c.tab_count += c.def_tab_incr
	new_tab_space := strings.Repeat(" ", c.tab_count)
	c.tab_space = new_tab_space
}

func (c *CodeGenerator) Dedent() {
	c.tab_space = c.prev_tab_space
	c.tab_count -= c.def_tab_incr
}

func (c *CodeGenerator) Write(content string, use_tab bool, add_space bool) {
	if use_tab {
		if add_space {
			c.final_str += c.tab_space + " " + content
		} else {
			c.final_str += c.tab_space + content
		}
	} else {
		if add_space {
			c.final_str += " " + content
		} else {
			c.final_str += content
		}
	}
}

func (c *CodeGenerator) NewLine() {
	c.Write("\n", false, false)
}

func (c *CodeGenerator) WriteLine(line string, use_tab bool) {
	if use_tab {
		c.final_str += c.tab_space + line + "\n"
	} else {
		c.final_str += line + "\n"
	}
}

func (c *CodeGenerator) IndentThenWriteline(line string) {
	c.Indent()
	c.WriteLine(line, true)
	c.Dedent()
}

func (c *CodeGenerator) CollectString() string {
	return c.final_str
}

func (c *CodeGenerator) AddInclude(include string) {
	if inc, ok := c.includes[include]; ok {
		c.includes[include] = inc + 1
	} else {
		c.includes[include] = 1
	}
}

func (c *CodeGenerator) GetIncludesAsString() string {
	var includes strings.Builder
	for inc := range c.includes {
		includes.WriteString("#include " + inc + "\n")
	}

	return includes.String()
}

func generate_code_from_block_expr(c *CodeGenerator, blk *BlockExpr) {
	for index, node := range blk.Contents {
		c.Indent()
		if index+1 >= len(blk.Contents) {
			switch node.(type) {
			case Expression:
				c.Write("return ", true, false)
				node.AsCppCode(c, false, false)
				c.WriteLine(";", false)
			case *Return:
				node.AsCppCode(c, true, false)
				c.NewLine()
			default:
				node.AsCppCode(c, true, true)
				c.WriteLine("return Proto_Unit();", true)
			}
		} else {
			node.AsCppCode(c, true, true)
		}
		c.Dedent()
	}

	if len(blk.Contents) == 0 {
		c.IndentThenWriteline("return Proto_Unit();")
	}
}
