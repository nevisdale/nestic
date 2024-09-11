package nes

import (
	"image"
	"image/color"
)

type PPU struct {
	paletteRam    [32]uint8      // 32 bytes of palette RAM
	nametablesRam [2][1024]uint8 // 2 nametables, 1024 bytes each

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

	bufferedData uint8 // buffered data to emulate 1 cycle delay

	v uint16 // current vram address
	t uint16 // temporary vram address
	x uint8  // fine x scroll
	w uint8  // first or second write toggle

	bgNextTileId  uint8
	bgNextAttr    uint8
	bgNextTileLsb uint8
	bgNextTileMsb uint8
}

func NewPPU(mem ReadWriter) *PPU {
	p := &PPU{
		screen:     image.NewRGBA(image.Rect(0, 0, 256, 240)),
		mem:        mem,
		paletteRam: [32]uint8{},
	}
	p.Reset()
	return p
}

func (p *PPU) Reset() {
	p.writeCtrl(0)
	p.writeMask(0)
	for i := 0; i < len(p.paletteRam); i++ {
		p.paletteRam[i] = 0
	}
	p.bufferedData = 0
	p.cycle = 0
	p.scanline = 0
	for i := 0; i < len(p.screen.Pix); i++ {
		p.screen.Pix[i] = 0
	}
}

func (p PPU) readRegister(addr uint16) uint8 {
	switch addr {
	case 0x2:
		data := p.readStatus()
		p.status.verticalBlank = 0
		data |= p.bufferedData & 0x1F // 3 firts bits are unused
		p.w = 0
		return data
	case 0x7:
		// emulate 1 cycle delay
		data := p.bufferedData
		p.bufferedData = p.mem.Read8(p.v)

		// palette read doesn't need to be delayed
		if p.v >= 0x3F00 {
			data = p.bufferedData
		}

		if p.ctrl.vramInc == 0 {
			p.v++
		} else {
			p.v += 32
		}
		return data
	}
	return 0
}

func (p *PPU) writeRegister(addr uint16, data uint8) {
	switch addr {
	case 0x0:
		p.writeCtrl(data)
		// t: ....BA.. ........ = d: ......BA
		p.t = p.t&0xF3FF | uint16(data&0x3)<<10
	case 0x1:
		p.writeMask(data)
	case 0x5:
		if p.w == 0 {
			// t: ....... ...ABCDE <- d: ABCDE...
			// x:              FGH <- d: .....FGH
			// w:                  <- 1
			p.t = p.t&0xFFE0 | uint16(data)>>3
			p.x = data & 0x7
			p.w = 1
		} else {
			// t: FGH..AB CDE..... <- d: ABCDEFGH
			// w:                  <- 0
			p.t = p.t&0x8C1F | uint16(data&0x7)<<12 | uint16(data&0xF8)<<2
			p.w = 0
		}
	case 0x6:
		if p.w == 0 {
			// t: .CDEFGH ........ <- d: ..CDEFGH
			//    <unused>         <- d: AB......
			// t: Z...... ........ <- 0 (bit Z is cleared)
			// w:                  <- 1
			// 10000000 11111111 = 0x
			p.t = (p.t & 0x80ff) | (uint16(data)&0x3F)<<8
			p.w = 1
		} else {
			// t: ....... ABCDEFGH <- d: ABCDEFGH
			// v: <...all bits...> <- t: <...all bits...>
			// w:                  <- 0
			p.t = (p.t & 0xff00) | uint16(data)
			p.v = p.t
			p.w = 0
		}
	case 0x7:
		p.mem.Write8(p.v, data)
		if p.ctrl.vramInc == 0 {
			p.v++
		} else {
			p.v += 32
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
	return status
}

func (p *PPU) readNametable(index uint8, addr uint16) uint8 {
	return p.nametablesRam[index][addr]
}

func (p *PPU) writeNametable(index uint8, addr uint16, data uint8) {
	p.nametablesRam[index][addr] = data
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
	// TODO: how to increment cycle and scanline?

	if p.scanline >= -1 && p.scanline < 240 {
		if p.scanline == -1 && p.cycle == 1 {
			p.status.verticalBlank = 0
		}

		if (p.cycle >= 2 && p.cycle < 258) || (p.cycle >= 321 && p.cycle < 338) {
			switch (p.cycle - 1) % 8 {
			case 0:
				p.bgNextTileId = p.mem.Read8(0x2000 | p.v&0x0FFF)
			case 2:
				nametableX := p.v & 0x0400
				nametableY := p.v & 0x0800
				coarseX := p.v & 0x001F >> 2
				coarseY := p.v & 0x03E0 >> 5
				p.bgNextAttr = p.mem.Read8(0x23C0 | nametableY | nametableX | coarseY<<3 | coarseX)
				if coarseY&0x2 != 0 {
					p.bgNextAttr >>= 4
				}
				if coarseX&0x2 != 0 {
					p.bgNextAttr >>= 2
				}
				p.bgNextAttr &= 0x3

			case 4:
				p.bgNextTileLsb = p.mem.Read8(uint16(p.ctrl.bgTable)<<12 + uint16(p.bgNextTileId)<<4 + uint16(p.v&0x7000)>>12)
			case 6:
				p.bgNextTileMsb = p.mem.Read8(uint16(p.ctrl.bgTable)<<12 + uint16(p.bgNextTileId)<<4 + uint16(p.v&0x7000)>>12 + 8)
			case 7:
				if p.mask.bg != 0 || p.mask.spr != 0 {
					p.incrementX()
				}
			}
		}
		if p.cycle == 256 {
			if p.mask.bg != 0 || p.mask.spr != 0 {
				p.incrementY()
			}
		}

		if p.cycle == 257 {
			if p.mask.bg != 0 || p.mask.spr != 0 {
				p.copyX()
			}
		}

		if p.scanline == -1 && p.cycle >= 280 && p.cycle < 305 {
			if p.mask.bg != 0 || p.mask.spr != 0 {
				p.copyY()
			}
		}
	}

	if p.scanline == 241 && p.cycle == 1 {
		p.status.verticalBlank = 1
		if p.ctrl.nmiEnable == 1 {
			p.nmi = true
		}
	}

	if p.scanline == -1 && p.cycle == 1 {
		p.status.verticalBlank = 0
	}

	// p.screen.Set(p.cycle-1, p.scanline, p.GetColorFromPalette(0, ))

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

func (p *PPU) GetBgTileById(palette, index uint8) *image.RGBA {
	addr := uint16(index) * 16
	if p.ctrl.bgTable == 1 {
		addr += 0x1000
	}

	t := tile{}

	for i := 0; i < 8; i++ {
		lsb := p.mem.Read8(addr)
		t[i][7] = uint8(lsb >> 0 & 1)
		t[i][6] = uint8(lsb >> 1 & 1)
		t[i][5] = uint8(lsb >> 2 & 1)
		t[i][4] = uint8(lsb >> 3 & 1)
		t[i][3] = uint8(lsb >> 4 & 1)
		t[i][2] = uint8(lsb >> 5 & 1)
		t[i][1] = uint8(lsb >> 6 & 1)
		t[i][0] = uint8(lsb >> 7 & 1)
		addr++
	}

	for i := 0; i < 8; i++ {
		msb := p.mem.Read8(addr)
		t[i][7] |= uint8(msb >> 0 & 1 << 1)
		t[i][6] |= uint8(msb >> 1 & 1 << 1)
		t[i][5] |= uint8(msb >> 2 & 1 << 1)
		t[i][4] |= uint8(msb >> 3 & 1 << 1)
		t[i][3] |= uint8(msb >> 4 & 1 << 1)
		t[i][2] |= uint8(msb >> 5 & 1 << 1)
		t[i][1] |= uint8(msb >> 6 & 1 << 1)
		t[i][0] |= uint8(msb >> 7 & 1 << 1)
		addr++
	}

	img := image.NewRGBA(image.Rect(0, 0, 8, 8))
	for y := 0; y < 8; y++ {
		for x := 0; x < 8; x++ {
			color := p.GetColorFromPalette(palette, t[y][x])
			img.Set(x, y, color)
		}
	}

	return img
}

func (p *PPU) GetPatternTable(palette, index uint8) *image.RGBA {
	if index > 1 {
		return nil
	}

	addr := uint16(index) * 0x1000
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

func (p *PPU) incrementX() {
	if p.v&0x001F == 31 { // if coarse X == 31
		p.v &= ^uint16(0x001F) // coarse X = 0
		p.v ^= 0x0400          // switch horizontal nametable
	} else {
		p.v += 1 // increment coarse X
	}
}

func (p *PPU) incrementY() {
	if p.v&0x7000 != 0x7000 { // if fine Y < 7
		p.v += 0x1000 // increment fine Y
	} else {
		p.v &= ^uint16(0x7000)   // fine Y = 0
		y := (p.v & 0x03E0) >> 5 // let y = coarse Y
		if y == 29 {
			y = 0         // coarse Y = 0
			p.v ^= 0x0800 // switch vertical nametable
		} else if y == 31 {
			y = 0 // coarse Y = 0, nametable not switched
		} else {
			y += 1 // increment coarse Y
		}
		p.v = (p.v & ^uint16(0x03E0)) | (y << 5) // put coarse Y back into v
	}
}

func (ppu *PPU) copyX() {
	ppu.v = ppu.v&0xFBE0 | ppu.t&0x041F
}

func (ppu *PPU) copyY() {
	ppu.v = ppu.v&0x841F | ppu.t&0x7BE0
}
