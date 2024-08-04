package nes

import "log"

// TODO: think about separating into TranslateCpuAddr and TranslatePpuAddr
type Mapper interface {
	ReadWriter
}

func NewMapper(cart *Cart) Mapper {
	switch cart.mapperID {
	case 0:
		return &Mapper0{cart}
	}
	return nil
}

type Mapper0 struct {
	cart *Cart
}

func (m Mapper0) mapAddr(addr uint16) uint16 {
	switch {
	case addr <= 0x1FFF:
		return addr
	case addr >= 0x8000 && addr <= 0xFFFF:
		if m.cart.pgrBanks > 1 {
			return addr & 0x7FFF
		}
		return addr & 0x3FFF
	}
	log.Printf("unhandled mapper0 address: %04X\n", addr)
	return 0
}

func (m Mapper0) Read8(addr uint16) uint8 {
	switch {
	// Read from CHR ROM
	case addr <= 0x1FFF:
		return m.cart.chrMem[m.mapAddr(addr)]
	// Read from PRG ROM
	case addr >= 0x8000 && addr <= 0xFFFF:
		return m.cart.pgrMem[m.mapAddr(addr)]
	}
	return 0
}

func (m *Mapper0) Write8(addr uint16, data uint8) {
	switch {
	// Write to CHR ROM
	case addr <= 0x1FFF:
	// Write to PRG ROM
	case addr >= 0x8000 && addr <= 0xFFFF:
		m.cart.pgrMem[m.mapAddr(addr)] = data
	}
}
