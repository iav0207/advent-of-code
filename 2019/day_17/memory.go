package main

type Memory struct {
	Program Code
	Heap    map[Word]Word
	Base    Word
}

func NewMemory(c Code) *Memory {
	return &Memory{Program: c, Heap: make(map[Word]Word)}
}

func (m Memory) GetAll(ps ...Param) (vals []Word) {
	if len(ps) > 0 {
		debugf("memory(base=%d):", m.Base)
		defer debugf("\n")
	}
	for _, p := range ps {
		val := m.Get(p)
		vals = append(vals, val)
		debugf("\t%s = %d", p, val)
	}
	return
}

func (m Memory) Get(p Param) Word {
	n := Word(len(m.Program))
	if p.Mode == Immediate {
		return p.Val
	}
	addr := p.Val
	if p.Mode == Relative {
		addr += m.Base
	}
	if addr < n {
		return m.Program[addr]
	}
	return m.Heap[addr]
}

func (m Memory) Put(dest Param, value Word) {
	if dest.Mode == Immediate {
		fatalf("Memory.Put: got literal destination %v\n", dest)
	}
	n := Word(len(m.Program))
	addr := dest.Val
	if dest.Mode == Relative {
		addr += m.Base
	}
	if addr < n {
		m.Program[addr] = value
	} else {
		m.Heap[addr] = value
	}
}
