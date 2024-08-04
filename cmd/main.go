package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/nevisdale/nestic/internal/nes"
)

var (
	romPath string
)

func main() {
	flag.StringVar(&romPath, "rom", "", "path to the ROM file")
	flag.Parse()

	cart, err := nes.NewCartFromFile(romPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "couldn't load the ROM: %s\n", err)
		os.Exit(1)
	}

	bus := nes.NewBus()
	bus.LoadCart(cart)
	bus.Reset()
}
