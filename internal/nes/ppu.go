package nes

import (
	"image"
	"image/color"
)

const (
	// CTRL
	ctrlNametable   uint8 = 1 << 0 // nametable address. 0: $2000; 1: $2400; 2: $2800; 3: $2C00
	ctrlVramInc     uint8 = 1 << 2 // vram address increment per CPU read/write of PPUDATA
	ctrlSprTable    uint8 = 1 << 3 // sprite pattern table address for 8x8 sprites. 0: $0000; 1: $1000
	ctrlBgTable     uint8 = 1 << 4 // background pattern table address. 0: $0000; 1: $1000
	ctrlSprSize     uint8 = 1 << 5 // sprite size. 0: 8x8; 1: 8x16
	ctrlMasterSlave uint8 = 1 << 6 // PPU master/slave select
	ctrlNmiEnable   uint8 = 1 << 7 // generate an NMI at the start of the vertical blanking interval

	// MASK
	maskGray uint8 = 1 << 0 // grayscale
	maskBgL  uint8 = 1 << 1 // show background in leftmost 8 pixels of screen
	maskSprL uint8 = 1 << 2 // show sprites in leftmost 8 pixels of screen
	maskBg   uint8 = 1 << 3 // show background
	maskSpr  uint8 = 1 << 4 // show sprites
	maskR    uint8 = 1 << 5 // emphasis red
	maskG    uint8 = 1 << 6 // emphasis green
	maskB    uint8 = 1 << 7 // emphasis blue

	// STATUS
	statusSprOverflow   uint8 = 1 << 5 // sprite overflow
	statusSprZeroHit    uint8 = 1 << 6 // sprite 0 hit
	statusVerticalBlank uint8 = 1 << 7 // vertical blank has started
)

type PPU struct {
	palette   [32]uint8      // 32 bytes of palette RAM
	nametable [2][1024]uint8 // 2 nametables, 1024 bytes each

	screen        *image.RGBA
	frameComplete bool
	cycle         int
	scanline      int

	ctrl   uint8 // PPUCTRL
	mask   uint8 // PPUMASK
	status uint8 // PPUSTATUS

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
		screen:    image.NewRGBA(image.Rect(0, 0, 256, 240)),
		mem:       mem,
		palette:   [32]uint8{},
		nametable: [2][1024]uint8{},
	}
	p.Reset()
	return p
}

func (p *PPU) Reset() {
}

func (p *PPU) readNametable(index uint8, addr uint16) uint8 {
	return p.nametable[index][addr]
}

func (p *PPU) writeNametable(index uint8, addr uint16, data uint8) {
	p.nametable[index][addr] = data
}

func (p *PPU) readPalette(addr uint16) uint8 {
	addr &= 0x1f
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1c {
		addr -= 0x10
	}
	return p.palette[addr]
}

func (p *PPU) writePalette(addr uint16, data uint8) {
	addr &= 0x1f
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1c {
		addr -= 0x10
	}
	p.palette[addr] = data
}

func (p *PPU) readRegister(addr uint16) uint8 {
	switch addr {
	case 0x2: // PPUSTATUS
		data := p.status
		p.status &= ^statusVerticalBlank // clear vblank flag
		data |= p.bufferedData & 0x1f
		p.w = 0
		return data

	case 0x7: // PPUDATA
		// emulate 1 cycle delay
		data := p.bufferedData
		p.bufferedData = p.mem.Read8(p.v)

		// palette read doesn't need to be delayed
		if p.v >= 0x3F00 {
			data = p.bufferedData
		}

		if p.ctrl&ctrlVramInc == 0 {
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
	case 0x0: // PPUCTRL
		p.ctrl = data
		// t: ....BA.. ........ = d: ......BA
		p.t = p.t&0xF3FF | uint16(data&0x3)<<10

	case 0x1: // PPUMASK
		p.mask = data

	case 0x5: // PPUSCROLL
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

	case 0x6: // PPUADDR
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

	case 0x7: // PPUDATA
		p.mem.Write8(p.v, data)
		if p.ctrl&ctrlVramInc == 0 {
			p.v++
		} else {
			p.v += 32
		}
	}
}

func (p *PPU) renderingEnabled() bool {
	return p.mask&(maskBg|maskSpr) != 0
}

func (p *PPU) Tic() {
	// defer func() {
	// 	fmt.Printf("scanline: %d, cycle: %d v: %04X t: %04X tile_id: %02X attr: %02X lsb: %02X msb: %02X\n",
	// 		p.scanline, p.cycle, p.v, p.t, p.bgNextTileId, p.bgNextAttr, p.bgNextTileLsb, p.bgNextTileMsb)
	// }()

	if p.scanline >= -1 && p.scanline < 240 {
		if p.scanline == -1 && p.cycle == 1 {
			p.status &= ^statusVerticalBlank // clear vblank flag
		}

		// TODO: move this to a separate function (fetchBackground)
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
				baseAddr := uint16(0)
				if p.ctrl&ctrlBgTable != 0 {
					baseAddr = 0x1000
				}
				p.bgNextTileLsb = p.mem.Read8(baseAddr + uint16(p.bgNextTileId)<<4 + uint16(p.v&0x7000)>>12)
			case 6:
				baseAddr := uint16(0)
				if p.ctrl&ctrlBgTable != 0 {
					baseAddr = 0x1000
				}
				p.bgNextTileMsb = p.mem.Read8(baseAddr + uint16(p.bgNextTileId)<<4 + uint16(p.v&0x7000)>>12 + 8)
			case 7:
				if p.renderingEnabled() {
					p.incrementX()
				}
			}
		}
		if p.cycle == 256 {
			if p.renderingEnabled() {
				p.incrementY()
			}
		}

		if p.cycle == 257 {
			if p.renderingEnabled() {
				p.transferX()
			}
		}

		if p.scanline == -1 && p.cycle >= 280 && p.cycle < 305 {
			if p.renderingEnabled() {
				p.transferY()
			}
		}
	}

	if p.scanline == 241 && p.cycle == 1 {
		p.status |= statusVerticalBlank
		if p.ctrl&ctrlNmiEnable != 0 {
			p.nmi = true
		}
	}

	if p.scanline == -1 && p.cycle == 1 {
		p.status &= ^statusSprOverflow
	}

	// p.screen.Set(p.cycle-1, p.scanline, p.GetColorFromPalette(0, ))

	// TODO: add odd frame skip
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

func (p *PPU) getTileById(tableId, tileId uint8) tile {
	addr := uint16(tableId)<<12 | uint16(tileId)<<4

	var t tile
	for i := 0; i < 8; i++ {
		lsb := p.mem.Read8(addr + 0)
		msb := p.mem.Read8(addr + 8)

		t[i][7] = (lsb>>0&1)<<0 | (msb>>0&1)<<1
		t[i][6] = (lsb>>1&1)<<0 | (msb>>1&1)<<1
		t[i][5] = (lsb>>2&1)<<0 | (msb>>2&1)<<1
		t[i][4] = (lsb>>3&1)<<0 | (msb>>3&1)<<1
		t[i][3] = (lsb>>4&1)<<0 | (msb>>4&1)<<1
		t[i][2] = (lsb>>5&1)<<0 | (msb>>5&1)<<1
		t[i][1] = (lsb>>6&1)<<0 | (msb>>6&1)<<1
		t[i][0] = (lsb>>7&1)<<0 | (msb>>7&1)<<1

		addr++
	}
	return t
}

func (p *PPU) GetBgTileById(palette, tileId uint8) *image.RGBA {
	tableId := uint8(0)
	if p.ctrl&ctrlBgTable != 0 {
		tableId = 1
	}
	t := p.getTileById(tableId, tileId)

	img := image.NewRGBA(image.Rect(0, 0, 8, 8))
	for y := 0; y < 8; y++ {
		for x := 0; x < 8; x++ {
			color := p.GetColorFromPalette(palette, t[y][x])
			img.SetRGBA(x, y, color)
		}
	}

	return img
}

func (p *PPU) GetPatternTable(palette, index uint8) *image.RGBA {
	if index > 1 {
		return nil
	}

	img := image.NewRGBA(image.Rect(0, 0, 128, 128))
	tileSize := 8
	for i := 0; i < 256; i++ {
		t := p.getTileById(index, uint8(i))
		for y := 0; y < 8; y++ {
			for x := 0; x < 8; x++ {
				color := p.GetColorFromPalette(palette, t[y][x])
				img.SetRGBA(x+i%16*tileSize, y+i/16*tileSize, color)
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
	const (
		coarseMask uint16 = 0x001f
		ntMask     uint16 = 0x0400
	)

	if p.v&coarseMask == coarseMask { // if coarse X == 31
		p.v &= ^coarseMask // coarse X = 0
		p.v ^= ntMask      // switch horizontal nametable
	} else {
		p.v += 1 // increment coarse X
	}
}

// TODO: add consts for these magic numbers
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

func (ppu *PPU) transferX() {
	const (
		xMask uint16 = 0x041f
		xClr  uint16 = ^xMask
	)
	//                           0000100 00011111 = xMask
	// v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF
	ppu.v = ppu.v&xClr | ppu.t&xMask
}

func (ppu *PPU) transferY() {
	const (
		yMask uint16 = 0x7be0
		yClr  uint16 = ^yMask
	)
	//                          01111011 11100000 = yMask
	// v: GHIA.BC DEF..... <- t: GHIA.BC DEF.....
	ppu.v = ppu.v&yClr | ppu.t&yMask
}
