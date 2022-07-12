package compiler

type Symbol struct {
	Name       string
	ScopeDepth int
	Index      int
}

type SymbolTable struct {
	store              map[string]*Symbol
	numOfStoredSymbols int
	CurScopeDepth      int
	EnclosingSymTable  *SymbolTable
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		store:              map[string]*Symbol{},
		numOfStoredSymbols: 0,
		CurScopeDepth:      0,
		EnclosingSymTable:  nil,
	}
}

func NewSymbolTableFrom(enclosing *SymbolTable) *SymbolTable {
	return &SymbolTable{
		store:              map[string]*Symbol{},
		numOfStoredSymbols: 0,
		CurScopeDepth:      enclosing.CurScopeDepth + 1,
		EnclosingSymTable:  enclosing,
	}
}

func (st *SymbolTable) Define(name string) (*Symbol, bool) {
	for _, s := range st.store {
		if s.Name == name {
			return s, true
		}
	}

	sym := &Symbol{
		Name:       name,
		ScopeDepth: st.CurScopeDepth,
		Index:      st.numOfStoredSymbols,
	}
	st.numOfStoredSymbols++
	st.store[name] = sym
	return sym, false
}

func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {
	sym, ok := st.store[name]

	if !ok && st.EnclosingSymTable != nil {
		return st.EnclosingSymTable.Resolve(name)
	}
	return sym, ok
}
