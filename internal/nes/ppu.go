package nes

type PPU struct {
	// Registers
	ppuctrl   uint8
	ppumask   uint8
	ppustatus uint8
	oamaddr   uint8
	oamdata   uint8
	ppuscroll uint8
	ppuaddr   uint8
	ppudata   uint8
	oamdma    uint8

	tableNames   [2][0x400]uint8
	tablePallete [0x20]uint8
	// TODO: it stored in the cart. should we use it here?
	tablePatterns [2][0x1000]uint8

	cycles   uint16
	scanLine uint16
	frame    uint16
}

func NewPPU() *PPU {
	return &PPU{}
}

func (p PPU) readRegister(addr uint16) uint8 {
	switch addr {
	case 0x0:
		return p.ppuctrl
	case 0x1:
		return p.ppumask
	case 0x2:
		return p.ppustatus
	case 0x3:
		return p.oamaddr
	case 0x4:
		return p.oamdata
	case 0x5:
		return p.ppuscroll
	case 0x6:
		return p.ppuaddr
	case 0x7:
		return p.ppudata
	default:
		return 0
	}
}

func (p *PPU) writeRegister(addr uint16, data uint8) {
	switch addr {
	case 0x2000:
		p.ppuctrl = data
	case 0x2001:
		p.ppumask = data
	case 0x2002:
		p.ppustatus = data
	case 0x2003:
		p.oamaddr = data
	case 0x2004:
		p.oamdata = data
	case 0x2005:
		p.ppuscroll = data
	case 0x2006:
		p.ppuaddr = data
	case 0x2007:
		p.ppudata = data
	}
}

func (p *PPU) Tic() {
	p.cycles++
	if p.cycles > 340 {
		p.cycles = 0
		p.scanLine++

		if p.scanLine > 260 {
			p.scanLine = 0 // or -1?
			p.frame++
		}
	}
}
