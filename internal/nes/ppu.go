package nes

type PPU struct {
	ppuctrl struct {
		N uint8 // nametable: 0: $2000, 1: $2400, 2: $2800, 3: $2C00
		I uint8 // increment: 0: add 1, 1: add 32
		S uint8 // sprite pattern table: 0: $0000, 1: $1000, ignored in 8x16 mode
		B uint8 // background pattern table: 0: $0000, 1: $1000
		H uint8 // sprite size: 0: 8x8, 1: 8x16
		P uint8 // ppu master/slave: 0: read backdrop from ext pins, 1: output color on ext pins
		V uint8 // nmi enable: 0: off, 1: on
	}

	ppumask struct {
		g uint8 // greyscale: 0: color, 1: greyscale
		m uint  // background left column: 0: hide, 1: show
		M uint  // sprites left column: 0: hide, 1: show
		b uint  // background: 0: hide, 1: show
		s uint  // sprites: 0: hide, 1: show
		R uint8 // red intensity: 0: normal, 1: emphasize
		G uint8 // green intensity: 0: normal, 1: emphasize
		B uint8 // blue intensity: 0: normal, 1: emphasize
	}

	ppustatus struct {
		_ [5]uint8
		O uint8 // sprite overflow: 0: no overflow, 1: overflow
		S uint8 // sprite 0 hit: 0: no hit, 1: hit
		V uint8 // vblank: 0: not in vblank, 1: in vblank

	}
	oamaddr   uint8  // oam address
	oamdata   uint8  // oam data
	ppuscroll uint8  // ppu scroll. first write is x, second write is y
	ppuaddr   uint16 // ppu address. first write is high byte, second write is low byte
	ppudata   uint8  // ppu data
	oamdma    uint8  // oam dma

	// Internal registers
	v uint16 // current vram address
	t uint16 // temporary vram address
	x uint8  // fine x scroll
	w uint8  // write toggle

	tableNames   [2][0x400]uint8
	tablePallete [0x20]uint8
	// TODO: it stored in the cart. should we use it here?
	tablePatterns [2][0x1000]uint8

	oam [0x100]uint8 // Object Attribute Memory

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
	case 0x1:
	case 0x2:
	case 0x3:
	case 0x4:
	case 0x5:
	case 0x6:
	case 0x7:
	}
	return 0
}

func (p *PPU) writeRegister(addr uint16, data uint8) {
	_ = data
	switch addr {
	case 0x0:
	case 0x1:
	case 0x2:
	case 0x3:
	case 0x4:
	case 0x5:
	case 0x6:
	case 0x7:
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
