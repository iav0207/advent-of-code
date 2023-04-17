package main

import (
	"fmt"
)

func main() {
	var fuelRequiredNetto, fuelRequiredBrutto int64
	var mass int64
	for {
		_, err := fmt.Scanf("%d\n", &mass)
		if err != nil {
			break
		}
		fuelForModule := mass/3 - 2

		fuelRequiredNetto += fuelForModule
		fuelRequiredBrutto += calcBrutto(fuelForModule)
	}
	fmt.Printf("Netto fuel required: %d\n", fuelRequiredNetto)
	fmt.Printf("Brutto fuel required: %d\n", fuelRequiredBrutto)
}

func calcBrutto(netto int64) (brutto int64) {
	brutto = netto
	inc := netto
	for {
		inc = inc/3 - 2
		if inc < 0 {
			break
		}
		brutto += inc
	}
	return
}
