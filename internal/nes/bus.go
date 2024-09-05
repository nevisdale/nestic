package nes

import (
	"image"
	"image/color"
	"strings"
)

type DebugInfo struct {
	PC uint16
	A  uint8
	X  uint8
	Y  uint8
	P  uint8
	SP uint8
}

func (d DebugInfo) StatusString() string {
	getByte := func(flag uint8, b byte) byte {
		if d.P&(1<<flag) > 0 {
			return b - ('a' - 'A')
		}
		return b
	}

	var status strings.Builder
	status.WriteByte(getByte(flagN, 'n'))
	status.WriteByte(getByte(flagV, 'v'))
	status.WriteByte(getByte(flagU, 'u'))
	status.WriteByte(getByte(flagB, 'b'))
	status.WriteByte(getByte(flagD, 'd'))
	status.WriteByte(getByte(flagI, 'i'))
	status.WriteByte(getByte(flagZ, 'z'))
	status.WriteByte(getByte(flagC, 'c'))
	return status.String()
}

type Bus struct {
	cpu  *CPU
	ppu  *PPU
	ram  *RAM
	cart *Cart

	ticCounter uint64

	stopped        bool
	oneStepAndStop bool
}

func NewBus() *Bus {
	b := &Bus{}
	b.ram = NewRAM()
	b.cpu = NewCPU(b.newCpuMemory())
	b.ppu = NewPPU(b.newPpuMemory())
	return b
}

func (b *Bus) TooglePause() {
	b.stopped = !b.stopped
}

func (b *Bus) LoadCart(cart *Cart) {
	b.cart = cart
	b.cpu.Reset()
}

func (b *Bus) GetPatternTable(palette, index uint8) *image.RGBA {
	return b.ppu.GetPatternTable(palette, index)
}

func (b *Bus) GetColorFromPalette(palette, index uint8) color.RGBA {
	return b.ppu.GetColorFromPalette(palette, index)
}

func (b *Bus) Screen() *image.RGBA {
	return b.ppu.screen
}

func (b *Bus) Reset() {
	b.cpu.Reset()
	b.ticCounter = 0
}

func (b *Bus) OneStepAndStop() {
	b.oneStepAndStop = true
	b.stopped = false
}

func (b *Bus) Disassemble() map[uint16]string {
	return b.cpu.Disassemble()
}

func (b *Bus) Tic() {
	if b.stopped {
		return
	}

	tic := func() uint8 {
		cycles := b.cpu.Tic()

		for range 3 * cycles {
			b.ppu.Tic()
		}
		b.ticCounter++

		if b.ppu.nmi {
			b.cpu.NMI()
			b.ppu.nmi = false
		}

		return cycles
	}
	for !b.ppu.frameComplete {
		cycles := tic()

		if b.oneStepAndStop && cycles == 0 {
			b.oneStepAndStop = false
			b.stopped = true
			return
		}
	}
	b.ppu.frameComplete = false
}

func (b *Bus) DebugInfo() DebugInfo {
	return DebugInfo{
		PC: b.cpu.pc,
		A:  b.cpu.a,
		X:  b.cpu.x,
		Y:  b.cpu.y,
		P:  b.cpu.p,
		SP: b.cpu.sp,
	}
}
