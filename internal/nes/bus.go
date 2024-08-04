package nes

type Bus struct {
	cpu  *CPU
	ppu  *PPU
	ram  *RAM
	cart *Cart

	ticCounter uint64
}

func NewBus() *Bus {
	b := &Bus{}
	b.ram = NewRAM()
	b.cpu = NewCPU(b.newCpuMemory())
	b.ppu = NewPPU()
	return b
}

func (b *Bus) LoadCart(cart *Cart) {
	b.cart = cart
	b.cpu.Reset()
}

func (b *Bus) Reset() {
	b.cpu.Reset()
	b.ticCounter = 0
}

func (b *Bus) Tic() {
	b.cpu.Tic()
	b.ppu.Tic()
	b.ticCounter++
}
