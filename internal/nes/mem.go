package nes

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
	case addr < 0x2000:
		return c.bus.ram.Read8(addr & 0x07FF)
	case addr < 0x4000:
		return c.bus.ppu.readRegister(addr & 0x7)
	case addr < 0x4018:
		return 0
	case addr < 0x4020:
		return 0
	case addr <= 0xFFFF:
		return c.bus.cart.Read8(addr)
	}
	return 0
}

func (c *cpuMemory) Write8(addr uint16, data uint8) {
	switch {
	case addr < 0x2000:
		c.bus.ram.Write8(addr&0x07FF, data)
	case addr < 0x4000:
		c.bus.ppu.writeRegister(addr&0x7, data)
	case addr < 0x4018:
	case addr < 0x4020:
	case addr <= 0xFFFF:
		c.bus.cart.Write8(addr, data)
	}
}

// PPU MEMORY MAP
//
// MIRRORS
// 0x4000 - 0x10000: Mirrors of 0x0000-0x4000
//
// PALLETTES
// 0x3F20 - 0x4000: Mirrors of 0x3F00-0x3F20
// 0x3F10 - 0x3F20: Sprite palette
// 0x3F00 - 0x3F10: Image palette
//
// NAME TABLES
// 0x3000 - 0x3F00: Mirrors of 0x2000-0x3000
// 0x2FC0 - 0x3000: Attribute table 3
// 0x2C00 - 0x2FC0: Name table 3
// 0x2BC0 - 0x2C00: Attribute table 2
// 0x2800 - 0x2BC0: Name table 2
// 0x27C0 - 0x2800: Attribute table 1
// 0x2400 - 0x27C0: Name table 1
// 0x23C0 - 0x2400: Attribute table 0
// 0x2000 - 0x23C0: Name table 0
//
// PATTERN TABLES
// 0x1000 - 0x2000: Pattern table 1
// 0x0000 - 0x1000: Pattern table 0
type ppuMemory struct {
	bus *Bus
}

func (b *Bus) newPpuMemory() *ppuMemory {
	return &ppuMemory{bus: b}
}

func (p ppuMemory) Read8(addr uint16) uint8 {
	addr &= 0x3FFF
	switch {
	case addr < 0x2000:
		return p.bus.cart.Read8(addr)

	case addr < 0x3F00:
		addr &= 0x0FFF

		// vertial:    0:0, 1:0, 2:1, 3:1
		// horizontal: 0:0, 1:1, 2:0, 3:1
		index := uint8(0)
		switch {
		case addr < 0x400: // nametable 0
			// the same for vertical and horizontal
			index = 0
		case addr < 0x800: // nametable 1
			// vertical (1) -> 0
			// horizontal (0) -> 1
			index = (^p.bus.cart.mirror) & 0x1
		case addr < 0xC00: // nametable 2
			// vertical (1) -> 1
			// horizontal (0) -> 0
			index = p.bus.cart.mirror
		case addr < 0x1000: // nametable 3
			// the same for vertical and horizontal
			index = 1
		}
		return p.bus.ppu.readNametable(index, addr&0x3FF)

	case addr < 0x4000:
		return p.bus.ppu.readPalette(addr)
	}
	return 0
}

func (p *ppuMemory) Write8(addr uint16, data uint8) {
	addr &= 0x3FFF
	switch {
	case addr < 0x3F00:
		addr &= 0x0FFF

		// vertial:    0:0, 1:0, 2:1, 3:1
		// horizontal: 0:0, 1:1, 2:0, 3:1
		index := uint8(0)
		switch {
		case addr < 0x400: // nametable 0
			// the same for vertical and horizontal
			index = 0
		case addr < 0x800: // nametable 1
			// vertical (1) -> 0
			// horizontal (0) -> 1
			index = (^p.bus.cart.mirror) & 0x1
		case addr < 0xC00: // nametable 2
			// vertical (1) -> 1
			// horizontal (0) -> 0
			index = p.bus.cart.mirror
		case addr < 0x1000: // nametable 3
			// the same for vertical and horizontal
			index = 1
		}
		addr = addr & 0x3FF
		p.bus.ppu.writeNametable(index, addr, data)

	case addr < 0x4000:
		p.bus.ppu.writePalette(addr, data)
	}
}
