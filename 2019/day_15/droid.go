package main

type Droid struct {
	Eval     Eval
	Position Point
	Field    Field
}

func NewDroid(c Code) *Droid {
	field := map[Point]Tile{Point{}: Open}
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

		d.Field[intent] = resp

		if resp != Wall {
			d.Position = intent
			d.Explore()
			d.Eval.In <- direc.Reverse().Code
			<-d.Eval.Out
			d.Position = init
		}
	}
}

func (d *Droid) BFSFrom(start Point) map[Point]Dist {
	dist := map[Point]Dist{start: 0}
	seen := map[Point]bool{start: true}
	todo := []Point{start}

	for len(todo) > 0 {
		this := todo[0]
		todo = todo[1:]
		for _, direc := range Directions {
			neighbor := this.Plus(direc.Delta)
			if neighborTile, ok := d.Field[neighbor]; !ok || neighborTile == Wall {
				continue
			} else if nDist, ok := dist[neighbor]; ok {
				newDist := nDist + 1
				if thisDist, ok := dist[this]; ok {
					newDist = min(newDist, thisDist)
				}
				dist[this] = newDist
				debugf("bfs: set %s dist=%d\n", this, newDist)
			} else if found := seen[neighbor]; !found {
				todo = append(todo, neighbor)
                seen[neighbor] = true
				debugf("bfs: enqueue %s\n", neighbor)
			}
		}
	}
	return dist
}

func min(a, b Dist) Dist {
	if a < b {
		return a
	}
	return b
}
