package main

import (
	"flag"
	"fmt"
	"os"
	"time"

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

	nes := nes.NewBus()
	nes.LoadCart(cart)
	nes.Reset()

	for {
		nes.Tic()
		time.Sleep(time.Second / 60)
	}

}
