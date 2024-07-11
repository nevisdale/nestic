package bus

import "github.com/nevisdale/nestic/internal/cpu"

const (
	// Detailed Memory Map:
	//
	// $0000-$07FF: Internal RAM
	//   This is the primary working RAM of the NES.
	//   It is 2KB in size and can store variables, stack, and other temporary data.
	//
	// $0800-$1FFF: Mirrors of $0000-$07FF
	//   Due to limited address space, the NES mirrors its internal RAM multiple times.
	//   Any write to these addresses will affect the corresponding address in the range $0000-$07FF.
	//
	// $2000-$2007: PPU Registers
	//   $2000: PPU Control Register 1
	//   $2001: PPU Control Register 2
	//   $2002: PPU Status Register
	//   $2003: SPR-RAM Address Register
	//   $2004: SPR-RAM I/O Register
	//   $2005: VRAM Address Register 1
	//   $2006: VRAM Address Register 2
	//   $2007: VRAM I/O Register
	//
	// $2008-$3FFF: Mirrors of $2000-$2007
	//   Similar to the internal RAM, the PPU registers are mirrored every 8 bytes.
	//
	// $4000-$4017: APU and I/O Registers
	//   These registers are used for audio processing and input/output control.
	//   Examples include sound channel control and joystick input.
	//
	// $4018-$401F: APU and I/O Functionality
	//   These addresses are typically disabled or used for expansion devices.
	//
	// $4020-$FFFF: Cartridge Space
	//   This space is allocated for the program ROM (PRG-ROM) and
	//   any additional memory or hardware provided by the cartridge.
	//
	// $6000-$7FFF: Cartridge RAM (optional, used for saving game data)
	//
	// $8000-$FFFF: PRG-ROM (contains the game’s code and data)
	ramSizeBytes = 0x10000
)

type Bus struct {
	ram [ramSizeBytes]uint8

	cpu *cpu.CPU
}

func NewBus() *Bus {
	return &Bus{}
}

func (b *Bus) ConnectCPU(cpu *cpu.CPU) {
	cpu.ConnectBus(b)
	b.cpu = cpu
}

func (b Bus) Read8(addr uint16) uint8 {
	return b.ram[addr]
}

func (b Bus) Read16(addr uint16) uint16 {
	return uint16(b.ram[addr]) | uint16(b.ram[addr+1])<<8
}

func (b Bus) Write8(addr uint16, data uint8) {
	b.ram[addr] = data
}
