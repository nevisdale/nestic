package nes

import "log"

type ReadWriter interface {
	Read8(addr uint16) uint8
	Write8(addr uint16, data uint8)
}

// $0000-$07FF: 2 KB of internal RAM
// $0800-$1FFF: Mirrors of $0000-$07FF
// $2000-$2007: PPU (Picture Processing Unit) registers
// $2008-$3FFF: Mirrors of $2000-$2007 (every 8 bytes)
// $4000-$4017: APU (Audio Processing Unit) and I/O registers
// $4018-$401F: APU and I/O functionality that is normally disabled
// $4020-$FFFF: Cartridge space, including PRG-ROM, PRG-RAM, and mapper registers
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
	_ = addr
	return 0
}

func (p *ppuMemory) Write8(addr uint16, data uint8) {
	addr &= 0x3FFF
	_ = addr
	_ = data
}
