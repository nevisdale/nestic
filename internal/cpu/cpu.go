package cpu

import "log"

type ReadWriter interface {
	Read8(addr uint16) uint8
	Read16(addr uint16) uint16
	Write8(addr uint16, data uint8)
	Write16(addr uint16, data uint16)
}

const (
	// The stack is located in the fixed memory page $0100 to $01FF.
	stackStartAddr = 0x1000
)

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

type addrMode uint8

const (
	// Immediate
	// Operand is a constant value.
	// Example: LDA #$10 (Load Accumulator with 10)
	addrModeIMM addrMode = iota + 1

	// Zero Page
	// Operand is located in the first 256 bytes of memory.
	// Example: LDA $10 (Load Accumulator from address $0010)
	addrModeZP

	// Zero Page, X
	// Operand address is in zero page plus the X register.
	// Example: LDA $10,X (Load Accumulator from address $0010 + X)
	addrModeZPX

	// Zero Page, Y
	// Operand address is in zero page plus the Y register.
	// Example: LDX $10,Y (Load X Register from address $0010 + Y)
	addrModeZPY

	// Absolute
	// Full 16-bit address.
	// Example: LDA $1234 (Load Accumulator from address $1234)
	addrModeABS

	// Absolute, X
	// Full 16-bit address plus the X register.
	// Example: LDA $1234,X (Load Accumulator from address $1234 + X)
	addrModeABSX

	// Absolute, Y
	// Full 16-bit address plus the Y register.
	// Example: LDA $1234,Y (Load Accumulator from address $1234 + Y)
	addrModeABSY

	// Indirect
	// Address is fetched from a pointer.
	// Example: JMP ($1234) (Jump to address stored at $1234)
	addrModeIND

	// Indexed Indirect (X)
	// Address is in zero page, indexed by X.
	// Example: LDA ($10,X) (Load Accumulator from address stored at $0010 + X)
	addrModeINDX

	// Indirect Indexed (Y)
	// Address is fetched from zero page pointer plus Y.
	// Example: LDA ($10),Y (Load Accumulator from address stored at $0010 + Y)
	addrModeINDY

	// Relative
	// Used for branching instructions.
	// The operand is a signed 8-bit offset from the current instruction’s address.
	// Example: BNE $10 (Branch if Not Equal, with an offset of $10)
	addrModeREL

	// Accumulator
	// Operand is the accumulator.
	// Example: LSR A (Logical Shift Right on Accumulator)
	addrModeACC

	// Implied
	// Operand is implicit.
	// Example: CLC (Clear Carry Flag)
	addrModeIMP
)

type opcodeFunc func()

type instruction struct {
	name   string
	fn     opcodeFunc
	mode   addrMode
	cycles uint8
}

type CPU struct {
	regA         uint8              // used to perform arithmetic and logical operations
	regX         uint8              // used primarily for indexing and temporary storage
	regY         uint8              // used mainly for indexing and temporary storage
	sp           uint8              // stack pointer
	pc           uint16             // program counter
	status       uint8              // contains flags from flagXBit
	bus          ReadWriter         // bus to read and write data
	instrs       [0x100]instruction // opcode -> instruction mapping
	cycles       uint8              // number of cycles left for the current operation
	addrMode     addrMode           // current address mode
	operandAddr  uint16             // address of the operand
	operandValue uint8              // value of the operand
	pageCrossed  bool               // flag to indicate if the operation crosses a page boundary
}

func NewCPU(rw ReadWriter) *CPU {
	c := &CPU{
		sp:  0xfd,
		bus: rw,
	}
	c.initInstructions()
	c.Reset()
	return c
}

func (c *CPU) ConnectBus(bus ReadWriter) {
	c.bus = bus
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

func (c *CPU) stackPop8() uint8 {
	c.sp++
	return c.bus.Read8(stackStartAddr + uint16(c.sp))
}

func (c *CPU) stackPop16() uint16 {
	lo := uint16(c.stackPop8())
	hi := uint16(c.stackPop8())
	return lo | hi<<8
}

func (c *CPU) stackPush8(data uint8) {
	c.bus.Write8(stackStartAddr+uint16(c.sp), data)
	c.sp--
}

func (c *CPU) stackPush16(data uint16) {
	lo := uint8(data & 0xff)
	hi := uint8(data >> 8)
	c.stackPush8(hi)
	c.stackPush8(lo)
}

func (c *CPU) Tic() {
	if c.cycles != 0 {
		c.cycles--
		return
	}

	opcode := c.bus.Read8(c.pc)
	c.pc++
	instr := c.instrs[opcode]
	c.fetch(instr.mode)
	instr.fn()
	c.cycles += instr.cycles

	c.addrMode = 0
	c.operandAddr = 0
	c.operandValue = 0
	c.pageCrossed = false
}

// Reset the CPU to its initial state
func (c *CPU) Reset() {
	c.regA = 0
	c.regX = 0
	c.regY = 0
	c.status = 0x00 | flagUBit
	c.sp = 0xfd
	c.pc = c.bus.Read16(0xfffc)
	c.cycles = 8
}

// interrupt request signal
func (c *CPU) IRQ() {
	if c.getFlag(flagIBit) {
		return
	}

	c.stackPush16(c.pc)
	c.setFlag(flagBBit, false)
	c.setFlag(flagUBit, true)
	c.setFlag(flagIBit, true)
	c.stackPush8(c.status)
	c.pc = c.bus.Read16(0xfffe)
	c.cycles = 7
}

// non-maskable interrupt request signal
func (c *CPU) NMI() {
	c.stackPush16(c.pc)
	c.setFlag(flagBBit, false)
	c.setFlag(flagUBit, true)
	c.setFlag(flagIBit, true)
	c.stackPush8(c.status)
	c.pc = c.bus.Read16(0xfffa)
	c.cycles = 8
}

func (c *CPU) fetch(addrMode addrMode) {
	c.addrMode = addrMode
	c.pageCrossed = false
	c.operandAddr = 0
	c.operandValue = 0

	switch addrMode {
	case addrModeIMM:
		c.operandAddr = c.pc
		c.pc++
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeZP:
		c.operandAddr = uint16(c.bus.Read8(c.pc))
		c.pc++
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeZPX:
		c.operandAddr = uint16(c.bus.Read8(c.pc) + c.regX)
		c.pc++
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeZPY:
		c.operandAddr = uint16(c.bus.Read8(c.pc) + c.regY)
		c.pc++
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeABS:
		c.operandAddr = c.bus.Read16(c.pc)
		c.pc += 2
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeABSX:
		baseAddr := c.bus.Read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.regX)
		c.operandValue = c.bus.Read8(c.operandAddr)
		c.pageCrossed = (baseAddr & 0xff00) != (c.operandAddr & 0xff00)

	case addrModeABSY:
		baseAddr := c.bus.Read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.regY)
		c.operandValue = c.bus.Read8(c.operandAddr)
		c.pageCrossed = (baseAddr & 0xff00) != (c.operandAddr & 0xff00)

	case addrModeIND:
		addr := c.bus.Read16(c.pc)
		c.pc += 2
		// simulate 6502 page boundary hardware bug
		// if low byte is 0x00FF, then high byte is 0x0000
		lo := addr
		hi := (lo & 0xff00) | (lo&0x00ff + 1)
		c.operandAddr = uint16(c.bus.Read8(lo)) | uint16(c.bus.Read8(hi))<<8
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeINDX:
		addr := uint16(c.bus.Read8(c.pc) + c.regX)
		c.pc++
		lo := uint16(c.bus.Read8(addr & 0x00ff))
		hi := uint16(c.bus.Read8((addr + 1) & 0x00ff))
		c.operandAddr = lo | hi<<8
		c.operandValue = c.bus.Read8(c.operandAddr)

	case addrModeINDY:
		addr := uint16(c.bus.Read8(c.pc))
		c.pc++
		lo := uint16(c.bus.Read8(addr))
		hi := uint16(c.bus.Read8((addr + 1) & 0x00ff))
		c.operandAddr = (lo | hi<<8) + uint16(c.regY)
		c.operandValue = c.bus.Read8(c.operandAddr)
		c.pageCrossed = (lo & 0xff00) != (hi & 0xff00)

	case addrModeREL:
		c.operandAddr = uint16(c.bus.Read8(c.pc))
		c.pc++
		if c.operandAddr&0x80 > 0 {
			c.operandAddr |= 0xFF00 // add leading 1 bits to save the sign
		}

	case addrModeACC:
		c.operandValue = c.regA

	case addrModeIMP:

	}

	log.Fatalln("unreachable. addr mode must be always valid")
}

// Add with Carry
// A = A + M + C
//
// Flags Affected: C, Z, N, V
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) adc() {
	r16 := uint16(c.regA) + uint16(c.operandValue)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16)
	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 > 0)
	c.setFlag(flagVBit, (c.regA^r8)&^(c.regA^c.operandValue)&0x80 != 0)
	if c.pageCrossed {
		c.cycles++
	}
	c.regA = r8
}

// Logical AND
// A = A & M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed
func (c *CPU) and() {
	c.regA &= c.operandValue
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Arithmetic Shift Left
// C <- (A or M)7, (A or M) << 1
//
// Flags affected: C, Z, N
func (c *CPU) asl() {
	r16 := uint16(c.operandValue) << 1
	r8 := uint8(r16)
	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 > 0)
	if c.addrMode == addrModeACC {
		c.regA = r8
	} else {
		c.bus.Write8(c.operandAddr, r8)
	}
}

// Branch if Carry Clear
// If C = 0, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bcc() {
	if c.getFlag(flagCBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Branch if Carry Set
// If C = 1, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bcs() {
	if !c.getFlag(flagCBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Branch if Equal
// If Z = 1, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) beq() {
	if !c.getFlag(flagZBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Bit Test
// A & M, N <- M7, V <- M6
//
// Flags affected: Z, N, V
func (c *CPU) bit() {
	m := c.regA & c.operandValue
	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0) // 1 << 7
	c.setFlag(flagVBit, m&0x40 > 0) // 1 << 6
}

// Branch if Minus
// If N = 1, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bmi() {
	if !c.getFlag(flagNBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Branch if Not Equal
// If Z = 0, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bne() {
	if c.getFlag(flagZBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Branch if Positive
// If N = 0, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bpl() {
	if c.getFlag(flagNBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Force Interrupt
//
// Flags affected: B, U
func (c *CPU) brk() {
	c.pc++
	c.stackPush16(c.pc)
	c.setFlag(flagIBit, true)
	c.setFlag(flagBBit, true)
	c.stackPush8(c.status)
	c.setFlag(flagBBit, false)
	c.pc = c.bus.Read16(0xfffe)
}

// Branch if Overflow Clear
// If V = 0, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bvc() {
	if c.getFlag(flagVBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Branch if Overflow Set
// If V = 1, PC <- PC + offset
//
// Flags affected: None
//
// An additional cycles:
//
//	If the branch occurs on the same page (1 cycle)
//	If the branch occurs on a different page (2 cycles)
func (c *CPU) bvs() {
	if !c.getFlag(flagVBit) {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
}

// Clear Carry Flag
// C <- 0
//
// Flags affected: C
func (c *CPU) clc() {
	c.setFlag(flagCBit, false)
}

// Clear Decimal Mode
// D <- 0
//
// Flags affected: D
func (c *CPU) cld() {
	c.setFlag(flagDBit, false)
}

// Clear Interrupt Disable
// I <- 0
//
// Flags affected: I
func (c *CPU) cli() {
	c.setFlag(flagIBit, false)
}

// Clear Overflow Flag
// V <- 0
//
// Flags affected: V
func (c *CPU) clv() {
	c.setFlag(flagVBit, false)
}

// Compare
// A - M
//
// Flags affected: C, Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) cmp() {
	r := c.regA - c.operandValue
	c.setFlag(flagCBit, c.regA >= c.operandValue)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Compare X Register
// X - M
//
// Flags affected: C, Z, N
func (c *CPU) cpx() {
	r := c.regX - c.operandValue
	c.setFlag(flagCBit, c.regX >= c.operandValue)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
}

// Compare Y Register
// Y - M
//
// Flags affected: C, Z, N
func (c *CPU) cpy() {
	r := c.regY - c.operandValue
	c.setFlag(flagCBit, c.regY >= c.operandValue)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
}

// Decrement Memory
// M - 1
//
// Flags affected: Z, N
func (c *CPU) dec() {
	r := c.operandValue - 1
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	c.bus.Write8(c.operandAddr, r)
}

// Decrement X Register
// X - 1
//
// Flags affected: Z, N
func (c *CPU) dex() {
	c.regX--
	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)
}

// Decrement Y Register
// Y - 1
//
// Flags affected: Z, N
func (c *CPU) dey() {
	c.regY--
	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)
}

// Exclusive OR
// A ^ M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) eor() {
	c.regA ^= c.operandValue
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Increment Memory
// M + 1
//
// Flags affected: Z, N
func (c *CPU) inc() {
	r := c.operandValue + 1
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	c.bus.Write8(c.operandAddr, r)
}

// Increment X Register
// X + 1
//
// Flags affected: Z, N
func (c *CPU) inx() {
	c.regX++
	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)
}

// Increment Y Register
// Y + 1
//
// Flags affected: Z, N
func (c *CPU) iny() {
	c.regY++
	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)
}

// Jump
// PC <- address
//
// Flags affected: None
func (c *CPU) jmp() {
	c.pc = c.operandAddr
}

// Jump to Subroutine
// PC <- address
//
// Flags affected: None
func (c *CPU) jsr() {
	c.pc-- // pc incremented by 1 after the fetch, so we need to decrement it
	c.stackPush16(c.pc)
	c.pc = c.operandAddr
}

// Load Accumulator
// A <- M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) lda() {
	c.regA = c.operandValue
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Load X Register
// X <- M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) ldx() {
	c.regX = c.operandValue
	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Load Y Register
// Y <- M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) ldy() {
	c.regY = c.operandValue
	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Logical Shift Right
// C <- (A or M)0, (A or M) >> 1
//
// Flags affected: C, Z, N
func (c *CPU) lsr() {
	c.setFlag(flagCBit, c.operandValue&0x1 > 0)
	r := c.operandValue >> 1
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	if c.addrMode == addrModeACC {
		c.regA = r
	} else {
		c.bus.Write8(c.operandAddr, r)
	}
}

// No Operation
// No operation is performed
//
// Flags affected: None
func (c *CPU) nop() {
}

// Logical Inclusive OR
// A | M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) ora() {
	c.regA |= c.operandValue
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Push Accumulator
// A -> stack
//
// Flags affected: None
func (c *CPU) pha() {
	c.stackPush8(c.regA)
}

// Push Processor Status
// status -> stack
func (c *CPU) php() {
	c.stackPush8(c.status)
}

// Pull Accumulator
// A <- stack
//
// Flags affected: Z, N
func (c *CPU) pla() {
	c.regA = c.stackPop8()
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
}

// Pull Processor Status
// status <- stack
//
// Flags affected: All
func (c *CPU) plp() {
	c.status = c.stackPop8()
}

// Rotate Left
// C <- (A or M)7, (A or M) << 1
//
// Flags affected: C, Z, N
func (c *CPU) rol() {
	carry := c.operandValue&0x80 > 0
	r := c.operandValue << 1
	if c.getFlag(flagCBit) {
		r |= 0x1
	}
	c.setFlag(flagCBit, carry)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	if c.addrMode == addrModeACC {
		c.regA = r
	} else {
		c.bus.Write8(c.operandAddr, r)
	}
}

// Rotate Right
// C -> (A or M)0, (A or M) >> 1
//
// Flags affected: C, Z, N
func (c *CPU) ror() {
	carry := c.operandValue&0x1 > 0
	r := c.operandValue >> 1
	if c.getFlag(flagCBit) {
		r |= 0x80
	}
	c.setFlag(flagCBit, carry)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
	if c.addrMode == addrModeACC {
		c.regA = r
	} else {
		c.bus.Write8(c.operandAddr, r)
	}
}

// Return from Interrupt
// status <- stack, PC <- stack
//
// Flags affected: All
func (c *CPU) rti() {
	c.status = c.stackPop8()
	c.status &= ^flagBBit
	c.status &= ^flagUBit
	c.pc = c.stackPop16()
}

// Return from Subroutine
// PC <- stack
//
// Flags affected: None
func (c *CPU) rts() {
	c.pc = c.stackPop16()
	c.pc++ // increment to the next instruction
}

// Subtract with Carry
// A = A - M - (1 - C)
//
// Flags affected: C, Z, N, V
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) sbc() {
	r16 := uint16(c.regA) + uint16(^c.operandValue)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16)
	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 > 0)
	c.setFlag(flagVBit, (r8^c.regA)&(r8^c.operandValue)&0x80 > 0)
	c.regA = r8
	if c.pageCrossed {
		c.cycles++
	}
}

// Set Carry Flag
// C <- 1
//
// Flags affected: C
func (c *CPU) sec() {
	c.setFlag(flagCBit, true)
}

// Set Decimal Flag
// D <- 1
//
// Flags affected: D
func (c *CPU) sed() {
	c.setFlag(flagDBit, true)
}

// Set Interrupt Disable
// I <- 1
//
// Flags affected: I
func (c *CPU) sei() {
	c.setFlag(flagIBit, true)
}

// Store Accumulator
// M <- A
//
// Flags affected: None
func (c *CPU) sta() {
	c.bus.Write8(c.operandAddr, c.regA)
}

// Store X Register
// M <- X
//
// Flags affected: None
func (c *CPU) stx() {
	c.bus.Write8(c.operandAddr, c.regX)
}

// Store Y Register
// M <- Y
//
// Flags affected: None
func (c *CPU) sty() {
	c.bus.Write8(c.operandAddr, c.regY)
}

// Transfer Accumulator to X
// X <- A
//
// Flags affected: Z, N
func (c *CPU) tax() {
	c.regX = c.regA
	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)
}

// Transfer Accumulator to Y
// Y <- A
//
// Flags affected: Z, N
func (c *CPU) tay() {
	c.regY = c.regA
	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)
}

// Transfer Stack Pointer to X
// X <- SP
//
// Flags affected: Z, N
func (c *CPU) tsx() {
	c.regX = c.sp
	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)
}

// Transfer X to Accumulator
// A <- X
//
// Flags affected: Z, N
func (c *CPU) txa() {
	c.regA = c.regX
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
}

// Transfer X to Stack Pointer
// SP <- X
//
// Flags affected: None
func (c *CPU) txs() {
	c.sp = c.regX
}

// Transfer Y to Accumulator
// A <- Y
//
// Flags affected: Z, N
func (c *CPU) tya() {
	c.regA = c.regY
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
}
