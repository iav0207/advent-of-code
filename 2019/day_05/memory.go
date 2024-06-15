package main

type Memory Code

func (m Memory) GetAll(ps ...Param) (refs []*Word) {
    debugf("memory:")
    for _, p := range ps {
        ref := m.Get(p)
        refs = append(refs, ref)
        debugf("\t%s = %d", p, *ref)
    }
    debugf("\n")
    return
}

func (m Memory) Get(p Param) *Word {
	switch p.Mode {
	case Position:
		return &m[p.Val]
	case Immediate:
		v := p.Val
		return &v
	}
	fatalf("Memory.Get(%v): unknown parameter mode\n", p.Mode)
	return nil
}
