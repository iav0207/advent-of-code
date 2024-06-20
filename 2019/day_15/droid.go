package main

type Droid struct {
	Eval     Eval
	Position Point
	Field    Field

	OxygenSystem Point
}

func NewDroid(c Code) *Droid {
	field := map[Point]*Tile{Point{}: &Tile{Type: Open}}
	return &Droid{Eval: *NewEval(c), Field: field}
}

func (d *Droid) Explore() {
	for _, direc := range Directions {
		init := d.Position
		intent := init.Plus(direc.Delta)
		if _, ok := d.Field[intent]; ok {
			continue
		}
		d.Eval.In <- direc.Code
		resp := <-d.Eval.Out

		d.Field[intent] = &Tile{Type: resp, Dist: 1 + d.Field[d.Position].Dist}

		if resp != Wall {
			d.Position = intent
			d.Explore()
			d.Eval.In <- direc.Reverse().Code
			<-d.Eval.Out
			d.Position = init
		}
	}
}

func (d *Droid) BFSFrom(start Point) {
	d.SetAllDists(-1)
	startTile := d.Field[start]
	startTile.Dist = 0
	queue := NewMemq(start)

	for !queue.Empty() {
		this, _ := queue.Next()
		thisTile := d.Field[this]
		for _, direc := range Directions {
			neighbor := this.Plus(direc.Delta)
			neighborTile, ok := d.Field[neighbor]
			if !ok || neighborTile.Type == Wall {
				continue
			}
			if neighborTile.Dist >= 0 {
				if thisTile.Dist < 0 {
					thisTile.Dist = neighborTile.Dist + 1
				} else {
					thisTile.Dist = min(thisTile.Dist, neighborTile.Dist+1)
				}
				debugf("bfs: set %s dist=%d\n", this, thisTile.Dist)
			}
			if neighborTile.Dist < 0 || neighborTile.Dist > thisTile.Dist {
				debugf("bfs: enqueue %s\n", neighbor)
				queue.Add(neighbor)
			}
		}
		if thisTile.Type == OxygenSystem {
			d.OxygenSystem = this
		}
	}
}

func (d *Droid) SetAllDists(dist int) {
	for _, tile := range d.Field {
		tile.Dist = dist
	}
}

func (d *Droid) MaxDist() (maxDist int) {
	for _, tile := range d.Field {
		if tile.Type != Wall && tile.Dist > maxDist {
			maxDist = tile.Dist
		}
	}
	return
}

type Memq struct {
	set  map[Point]bool
	todo []Point
}

func NewMemq(ps ...Point) *Memq {
	s := &Memq{set: make(map[Point]bool)}
	for _, p := range ps {
		s.Add(p)
	}
	return s
}

func (q *Memq) Add(p Point) {
	if _, ok := q.set[p]; !ok {
		q.todo = append(q.todo, p)
		q.set[p] = true
	}
}

func (q *Memq) Empty() bool { return len(q.todo) == 0 }

func (q *Memq) Next() (p Point, ok bool) {
	if ok = !q.Empty(); ok {
		p = q.todo[0]
		q.todo = q.todo[1:]
	}
	return
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
