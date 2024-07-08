package cpu

import (
	"encoding/csv"
)

type ReadWriter interface {
	Read(addr uint16, readOnly bool) uint8
	Write(addr uint16, data uint8)
}

const (
	flagCBit = uint8(1 << 0) // Carry flag
	flagZBit = uint8(1 << 1) // Zero flag
	flagIBit = uint8(1 << 2) // Interrupt Disable flag
	flagDBit = uint8(1 << 3) // Decimal Mode flag
	flagBBit = uint8(1 << 4) // Break Command flag
	flagUBit = uint8(1 << 5) // Unused
	flagVBit = uint8(1 << 6) // Overflow flag
	flagNBit = uint8(1 << 7) // Negative flag
)

type instruction struct {
	name     string
	operate  func()
	addrMode addrMode
	cycles   uint8
}

type CPU struct {
	regA uint8 // used to perform arithmetic and logical operations
	regX uint8 // used primarily for indexing and temporary storage
	regY uint8 // used mainly for indexing and temporary storage

	// The stack is located in the fixed memory page $0100 to $01FF.
	// The Stack Pointer holds the lower 8 bits of the address within this page
	//
	// Initialization:
	// The Stack Pointer is initialized by the system or the program at the start of execution.
	// Typically, it starts at $FF, pointing to the top of the stack.
	sp uint8

	pc     uint16 // program counter
	status uint8  // contains flags from flagXBit

	// bus to connect to RAM
	bus ReadWriter

	// Opcode matrix. see more https://www.masswerk.at/6502/6502_instruction_set.html
	//
	// Position in the slice is opcode.
	instructions []instruction
}

// must be in following format:
// opcode, mnemonic, address mode, cycles
func (c *CPU) SetInstructionFromCsv(r csv.Reader) error {
	return nil
}

func (c CPU) read(addr uint16) uint8 {
	return c.bus.Read(addr, true)
}

func (c *CPU) write(addr uint16, data uint8) {
	c.bus.Write(addr, data)
}

func (c CPU) getFlag(flag uint8) bool {
	return c.status&flag > 0
}

func (c *CPU) setFlag(flag uint8, v bool) {
	if v {
		c.status |= flag
		return
	}
	c.status &= ^flag
}
