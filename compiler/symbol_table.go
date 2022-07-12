package compiler

type Symbol struct {
	Name       string
	ScopeDepth int
	Index      int
}

type SymbolTable struct {
	store              []*Symbol
	numOfStoredSymbols int
	CurScopeDepth      int
	EnclosingSymTable  *SymbolTable
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		store:              []*Symbol{},
		numOfStoredSymbols: 0,
		CurScopeDepth:      0,
		EnclosingSymTable:  nil,
	}
}

func NewSymbolTableFrom(enclosing *SymbolTable) *SymbolTable {
	return &SymbolTable{
		store:              []*Symbol{},
		numOfStoredSymbols: 0,
		CurScopeDepth:      enclosing.CurScopeDepth + 1,
		EnclosingSymTable:  enclosing,
	}
}

func (st *SymbolTable) Define(name string) (*Symbol, bool) {
	for _, s := range st.store {
		if s.Name == name && s.ScopeDepth == st.CurScopeDepth {
			return s, true
		}
	}

	sym := &Symbol{
		Name:       name,
		ScopeDepth: st.CurScopeDepth,
		Index:      st.numOfStoredSymbols,
	}
	st.store = append(st.store, sym)
	st.numOfStoredSymbols++
	return sym, false
}

func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {
	for i := len(st.store) - 1; i >= 0; i-- {
		s := st.store[i]
		if s.Name == name && s.ScopeDepth <= st.CurScopeDepth {
			return s, true
		}
	}

	if st.EnclosingSymTable != nil {
		return st.EnclosingSymTable.Resolve(name)
	}
	return nil, false
}
