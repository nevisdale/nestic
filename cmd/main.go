package main

import (
	"log"
	"os"

	"github.com/nevisdale/nestic/internal/cpu"
)

func main() {
	cpu, err := cpu.NewCPU()
	if err != nil {
		log.Fatalf("couldn't create cpu: %s\n", err.Error())
		os.Exit(1)
	}

	_ = cpu
}
