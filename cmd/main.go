package main

import (
	"fmt"
	"log"
	"os"

	"github.com/nevisdale/nestic/internal/bus"
	"github.com/nevisdale/nestic/internal/cpu"
)

func main() {
	cpu, err := cpu.NewCPU()
	if err != nil {
		log.Fatalf("couldn't create cpu: %s\n", err.Error())
		os.Exit(1)
	}

	bus := bus.NewBus()
	bus.ConnectCPU(cpu)

	a := uint(12)
	fmt.Printf("a: %b\n", a)
	a = ^a
	fmt.Printf("a: %b\n", a)
}
