package nes

const ramSizeBytes = 0x800

type RAM struct {
	ram [ramSizeBytes]uint8
}

func NewRAM() *RAM {
	return &RAM{}
}

func (r *RAM) Read8(addr uint16) uint8 {
	return r.ram[addr]
}

func (r *RAM) Write8(addr uint16, data uint8) {
	r.ram[addr] = data
}
