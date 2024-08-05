package nes

import "log"

type ReadWriter interface {
	Read8(addr uint16) uint8
	Write8(addr uint16, data uint8)
}

// CPU MEMORY MAP
//
// PRG-ROM
// 0xC000 - 0x10000: Upper PRG-ROM bank
// 0x8000 - 0xC000: Lower PRG-ROM bank
//
// SRAM
// 0x6000 - 0x8000: SRAM
//
// EXPANSION ROM
// 0x4020 - 0x6000: Expansion ROM
//
// IO REGISTERS
// 0x4000 - 0x4020: I/O Registers
// 0x2008 - 0x4000: Mirrors of 0x2000-0x2008
// 0x2000 - 0x2008: PPU Registers
//
// RAM
// 0x0800 - 0x2000: Mirrors of 0x0000-0x0800
// 0x0200 - 0x0800: RAM
// 0x0100 - 0x0200: STACK
// 0x0000 - 0x0100: ZERO PAGE
type cpuMemory struct {
	bus *Bus
}

func (b *Bus) newCpuMemory() *cpuMemory {
	return &cpuMemory{bus: b}
}

func (c cpuMemory) Read8(addr uint16) uint8 {
	switch {
	// read from ram
	case addr < 0x2000:
		return c.bus.ram.Read8(addr & 0x07FF)
	// read from ppu
	case addr < 0x4000:
		return c.bus.ppu.readRegister(addr & 0x7)
	// read from apu
	case addr < 0x4018:
	// read from io
	case addr < 0x4020:
	// read from cartridge
	case addr <= 0xFFFF:
		return c.bus.cart.Read8(addr)
	}

	log.Fatalln("cpuMemory: unhandled read8 at address", addr)
	return 0
}

func (c *cpuMemory) Write8(addr uint16, data uint8) {
	switch {
	// write to ram
	case addr < 0x2000:
		c.bus.ram.Write8(addr&0x07FF, data)
		return
	// write to ppu
	case addr < 0x4000:
		c.bus.ppu.writeRegister(addr&0x7, data)
		return
	// write to apu
	case addr < 0x4018:
		return
	// write to io
	case addr < 0x4020:
		return
		// write to cartridge
	case addr <= 0xFFFF:
		c.bus.cart.Write8(addr, data)
		return
	}

	log.Fatalln("cpuMemory: unhandled write8 at address", addr)
}

// $0000-$0FFF: Pattern table 0
// $1000-$1FFF: Pattern table 1
// $2000-$23FF: Nametable 0
// $2400-$27FF: Nametable 1
// $2800-$2BFF: Nametable 2
// $2C00-$2FFF: Nametable 3
// $3000-$3EFF: Mirrors of $2000-$2FFF
// $3F00-$3F1F: Palette RAM indexes
// $3F20-$3FFF: Mirrors of $3F00-$3F1F
type ppuMemory struct {
	bus *Bus
}

func (b Bus) newPpuMemory() *ppuMemory {
	return &ppuMemory{bus: &b}
}

func (p ppuMemory) Read8(addr uint16) uint8 {
	addr &= 0x3FFF
	switch {
	case addr < 0x2000:
		return p.bus.cart.Read8(addr)
	case addr < 0x3F00:
	case addr < 0x4000:
		addr &= 0x1F
		// Palette mirroring
		if addr >= 0x10 {
			addr -= 0x10
		}
		return p.bus.ppu.tablePallete[addr]
	}
	return 0
}

func (p *ppuMemory) Write8(addr uint16, data uint8) {
	addr &= 0x3FFF
	switch {
	case addr < 0x2000:
		p.bus.cart.Write8(addr, data)
		return
	case addr < 0x3F00:
		return
	case addr < 0x4000:
		addr &= 0x1F
		// Palette mirroring
		if addr >= 0x10 {
			addr -= 0x10
		}
		p.bus.ppu.tablePallete[addr] = data
		return
	}
}
