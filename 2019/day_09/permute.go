package main

func permute(slotCount int, uniqueness int, values []Word) <-chan []Word {
	set := make(map[Word]int)
	for _, v := range values {
		set[v] = uniqueness
	}
	return permuteRec(set, slotCount)
}

func permuteRec(set map[Word]int, slotsLeft int) <-chan []Word {
	ch := make(chan []Word)
	go func() {
		defer close(ch)
		if slotsLeft == 0 {
			ch <- nil
			return
		}
		var canUse []Word
		for val, free := range set {
			if free > 0 {
				canUse = append(canUse, val)
			}
		}
		for _, val := range canUse {
			set[val]--
			for tail := range permuteRec(set, slotsLeft-1) {
				ch <- append(Words(val), tail...)
			}
			set[val]++
		}
	}()
	return ch
}
