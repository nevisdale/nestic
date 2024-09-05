package nes

import (
	"image"
	"image/color"
	v2 "math/rand/v2"
)

type PPU struct {
	paletteRam [32]uint8

	screen        *image.RGBA
	frameComplete bool
	cycle         int
	scanline      int

	ctrl struct {
		nametableX  uint8 // nametable address
		nametableY  uint8 // nametable address
		vramInc     uint8 // vram address increment per CPU read/write of PPUDATA
		sprTable    uint8 // sprite pattern table address for 8x8 sprites
		bgTable     uint8 // background pattern table address
		sprSize     uint8 // sprite size
		masterSlave uint8 // PPU master/slave select
		nmiEnable   uint8 // generate an NMI at the start of the vertical blanking interval
	}

	mask struct {
		grayscale uint8 // grayscale
		bgLeft    uint8 // show background in leftmost 8 pixels of screen
		sprLeft   uint8 // show sprites in leftmost 8 pixels of screen
		bg        uint8 // show background
		spr       uint8 // show sprites
		emphR     uint8 // emphasis red
		emphG     uint8 // emphasis green
		emphB     uint8 // emphasis blue
	}

	status struct {
		unused        uint8 // unused
		sprOverflow   uint8 // sprite overflow
		sprZeroHit    uint8 // sprite 0 hit
		verticalBlank uint8 // vertical blank has started
	}

	mem ReadWriter

	nmi bool

	address_latch uint8
	ppu_addr      uint16
	ppu_data_buf  uint8
}

func NewPPU(mem ReadWriter) *PPU {
	return &PPU{
		screen:     image.NewRGBA(image.Rect(0, 0, 256, 240)),
		mem:        mem,
		paletteRam: [32]uint8{},
	}
}

func (p PPU) readRegister(addr uint16) uint8 {
	switch addr {
	case 0x0: // ctrl. only write
	case 0x1: // mask. only write
	case 0x2:
		data := p.readStatus()
		data |= p.ppu_data_buf & 0x1F // 3 firts bits are unused
		p.address_latch = 0
		return data
	case 0x3:
	case 0x4:
	case 0x5:
	case 0x6:
		return 0
	case 0x7:
		// emulate 1 cycle delay
		data := p.ppu_data_buf
		p.ppu_data_buf = p.mem.Read8(p.ppu_addr)

		// palette read doesn't need to be delayed
		if p.ppu_addr >= 0x3F00 {
			data = p.ppu_data_buf
		}

		if p.ctrl.vramInc == 0 {
			p.ppu_addr++
		} else {
			p.ppu_addr += 32
		}

		return data
	}
	return 0
}

func (p *PPU) writeRegister(addr uint16, data uint8) {
	switch addr {
	case 0x0:
		p.writeCtrl(data)
	case 0x1:
		p.writeMask(data)
	case 0x2: // status. only read
	case 0x3:
	case 0x4:
	case 0x5:
	case 0x6:
		if p.address_latch == 0 {
			p.ppu_addr = p.ppu_addr&0x00FF | uint16(data)<<8
			p.address_latch = 1
		} else {
			p.ppu_addr = p.ppu_addr&0xFF00 | uint16(data)
			p.address_latch = 0
		}
	case 0x7:
		p.mem.Write8(p.ppu_addr, data)
		if p.ctrl.vramInc == 0 {
			p.ppu_addr++
		} else {
			p.ppu_addr += 32
		}
	}
}

func (p *PPU) writeCtrl(data uint8) {
	p.ctrl.nametableX = data >> 0 & 1
	p.ctrl.nametableY = data >> 1 & 1
	p.ctrl.vramInc = data >> 2 & 1
	p.ctrl.sprTable = data >> 3 & 1
	p.ctrl.bgTable = data >> 4 & 1
	p.ctrl.sprSize = data >> 5 & 1
	p.ctrl.masterSlave = data >> 6 & 1
	p.ctrl.nmiEnable = data >> 7 & 1
}

func (p *PPU) writeMask(data uint8) {
	p.mask.grayscale = data >> 0 & 1
	p.mask.bgLeft = data >> 1 & 1
	p.mask.sprLeft = data >> 2 & 1
	p.mask.bg = data >> 3 & 1
	p.mask.spr = data >> 4 & 1
	p.mask.emphR = data >> 5 & 1
	p.mask.emphG = data >> 6 & 1
	p.mask.emphB = data >> 7 & 1
}

func (p *PPU) readStatus() uint8 {
	var status uint8
	status |= p.status.unused
	status |= p.status.sprOverflow << 5
	status |= p.status.sprZeroHit << 6
	status |= p.status.verticalBlank << 7
	p.status.verticalBlank = 0
	return status
}

func (p *PPU) readPalette(addr uint16) uint8 {
	addr &= 0x1F
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1C {
		addr -= 0x10
	}
	return p.paletteRam[addr]
}

func (p *PPU) writePalette(addr uint16, data uint8) {
	addr &= 0x1F
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1C {
		addr -= 0x10
	}
	p.paletteRam[addr] = data
}

func (p *PPU) Tic() {
	if p.scanline == 241 && p.cycle == 1 {
		p.status.verticalBlank = 1
		if p.ctrl.nmiEnable == 1 {
			p.nmi = true
		}
	}

	if p.scanline == -1 && p.cycle == 1 {
		p.status.verticalBlank = 0
	}

	pixel := ColorPalette[0x3F]
	if v2.Int()&1 == 1 {
		pixel = ColorPalette[0x30]
	}
	p.screen.Set(p.cycle-1, p.scanline, pixel)

	p.cycle++
	if p.cycle >= 341 {
		p.cycle = 0
		p.scanline++
		if p.scanline >= 261 {
			p.scanline = -1
			p.frameComplete = true
		}
	}
}

type tile [8][8]uint8

func (p *PPU) GetPatternTable(palette, index uint8) *image.RGBA {
	if index > 1 {
		return nil
	}

	addr := uint16(index) * 0x1000 // check ppu memory map
	tiles := [256]tile{}

	for i := 0; i < 256; i++ {
		var t tile

		for j := 0; j < 8; j++ {
			lsb := p.mem.Read8(addr)
			t[j][7] = uint8(lsb >> 0 & 1)
			t[j][6] = uint8(lsb >> 1 & 1)
			t[j][5] = uint8(lsb >> 2 & 1)
			t[j][4] = uint8(lsb >> 3 & 1)
			t[j][3] = uint8(lsb >> 4 & 1)
			t[j][2] = uint8(lsb >> 5 & 1)
			t[j][1] = uint8(lsb >> 6 & 1)
			t[j][0] = uint8(lsb >> 7 & 1)
			addr++
		}

		for j := 0; j < 8; j++ {
			msb := p.mem.Read8(addr)
			t[j][7] |= uint8(msb >> 0 & 1 << 1)
			t[j][6] |= uint8(msb >> 1 & 1 << 1)
			t[j][5] |= uint8(msb >> 2 & 1 << 1)
			t[j][4] |= uint8(msb >> 3 & 1 << 1)
			t[j][3] |= uint8(msb >> 4 & 1 << 1)
			t[j][2] |= uint8(msb >> 5 & 1 << 1)
			t[j][1] |= uint8(msb >> 6 & 1 << 1)
			t[j][0] |= uint8(msb >> 7 & 1 << 1)
			addr++
		}

		tiles[i] = t
	}

	img := image.NewRGBA(image.Rect(0, 0, 128, 128))
	tileSize := 8
	for i := 0; i < 256; i++ {
		for y := 0; y < 8; y++ {
			for x := 0; x < 8; x++ {
				color := p.GetColorFromPalette(palette, tiles[i][y][x])
				img.Set(x+i%16*tileSize, y+i/16*tileSize, color)
			}
		}
	}

	return img
}

func (p *PPU) GetColorFromPalette(palette, pixel uint8) color.RGBA {
	addr := 0x3F00 + uint16(palette)<<2 + uint16(pixel)
	return ColorPalette[p.mem.Read8(addr)]
}
