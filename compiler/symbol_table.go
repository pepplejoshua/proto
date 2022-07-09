package compiler

type SymScope int

const (
	Global SymScope = iota
)

type Symbol struct {
	Name  string
	Scope SymScope
	Index int
}

type SymbolTable struct {
	store              map[string]*Symbol
	numOfStoredSymbols int
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		store:              map[string]*Symbol{},
		numOfStoredSymbols: 0,
	}
}

func (st *SymbolTable) Define(name string) *Symbol {
	for _, s := range st.store {
		if s.Name == name {
			return s
		}
	}
	sym := &Symbol{
		Name:  name,
		Scope: Global,
		Index: st.numOfStoredSymbols,
	}
	st.numOfStoredSymbols++
	st.store[name] = sym
	return sym
}

func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {
	sym, ok := st.store[name]
	return sym, ok
}
