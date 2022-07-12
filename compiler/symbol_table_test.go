package compiler

import "testing"

func TestDefiningSymbols(t *testing.T) {
	expected := map[string]*Symbol{
		"a": {
			Name:       "a",
			ScopeDepth: 0,
			Index:      0,
		},
		"b": {
			Name:       "b",
			ScopeDepth: 0,
			Index:      1,
		},
	}

	global := NewSymbolTable()

	a, _ := global.Define("a")
	if *a != *expected["a"] {
		t.Fatalf("Expected a=%+v, but got a=%+v", expected["a"], a)
	}

	b, _ := global.Define("b")
	if *b != *expected["b"] {
		t.Fatalf("Expected b=%+v, but got b=%+v", expected["b"], b)
	}
}

func TestResolvingSymbols(t *testing.T) {
	global := NewSymbolTable()

	global.Define("a")
	global.Define("b")
	global.Define("c")

	expected := []*Symbol{
		{
			Name:       "a",
			ScopeDepth: 0,
			Index:      0,
		},
		{
			Name:       "b",
			ScopeDepth: 0,
			Index:      1,
		},
		{
			Name:       "c",
			ScopeDepth: 0,
			Index:      2,
		},
	}

	for _, sym := range expected {
		if res, ok := global.Resolve(sym.Name); !ok {
			t.Errorf("'%s' is not resolvable", res.Name)
		} else {
			if *res != *sym {
				t.Errorf("expected %s to resolve to %+v but got %+v", sym.Name, sym, res)
			}
		}
	}
}
