package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/nevisdale/nestic/internal/nes"
	"github.com/nevisdale/nestic/internal/ui"
	"github.com/pkg/profile"
)

var (
	romPath      string
	pprofProfile string
)

func main() {
	flag.StringVar(&romPath, "rom", "", "path to the ROM file")
	flag.StringVar(&pprofProfile, "pprof", "", "profile type. cpu|mem")
	flag.Parse()

	if romPath == "" {
		fmt.Fprintf(os.Stderr, "usage: %s -rom <path to ROM file>\n", os.Args[0])
		os.Exit(1)
	}

	if pprofProfile != "" {
		switch pprofProfile {
		case "cpu":
			defer profile.Start(profile.CPUProfile, profile.ProfilePath(".")).Stop()
		case "mem":
			defer profile.Start(profile.MemProfile, profile.ProfilePath(".")).Stop()
		}
	}

	cart, err := nes.NewCartFromFile(romPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "couldn't load the ROM: %s\n", err)
		os.Exit(1)
	}

	nes := nes.NewBus()
	nes.LoadCart(cart)
	nes.Reset()

	window := ui.New(nes)
	if err := ui.RunUI(window); err != nil {
		log.Fatal(err)
	}
}
