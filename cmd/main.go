package main

import (
	"github.com/nevisdale/nestic/internal/bus"
	"github.com/nevisdale/nestic/internal/cpu"
)

func main() {
	bus := bus.NewBus()

	cpu := cpu.NewCPU(bus)
	bus.ConnectCPU(cpu)
}
