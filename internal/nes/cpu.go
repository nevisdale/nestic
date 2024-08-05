package nes

import (
	"log"
)

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

func (a addrMode) String() string {
	switch a {
	case addrModeIMM:
		return "IMM"
	case addrModeZP:
		return "ZP"
	case addrModeZPX:
		return "ZPX"
	case addrModeZPY:
		return "ZPY"
	case addrModeABS:
		return "ABS"
	case addrModeABSX:
		return "ABSX"
	case addrModeABSY:
		return "ABSY"
	case addrModeIND:
		return "IND"
	case addrModeINDX:
		return "INDX"
	case addrModeINDY:
		return "INDY"
	case addrModeREL:
		return "REL"
	case addrModeACC:
		return "ACC"
	case addrModeIMP:
		return "IMP"
	default:
		return "UNKNOWN"
	}
}

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
	a            uint8              // used to perform arithmetic and logical operations
	x            uint8              // used primarily for indexing and temporary storage
	y            uint8              // used mainly for indexing and temporary storage
	p            uint8              // contains flags from flagXBit
	sp           uint8              // stack pointer
	pc           uint16             // program counter
	mem          ReadWriter         // bus to read and write data
	instrs       [0x100]instruction // opcode -> instruction mapping
	cycles       uint8              // number of cycles left for the current operation
	addrMode     addrMode           // current address mode
	operandAddr  uint16             // address of the operand
	operandValue uint8              // value of the operand
	pageCrossed  bool               // flag to indicate if the operation crosses a page boundary
}

func NewCPU(mem ReadWriter) *CPU {
	c := &CPU{
		mem: mem,
	}
	c.initInstructions()
	return c
}

func (c CPU) read8(addr uint16) uint8 {
	return c.mem.Read8(addr)
}

func (c CPU) read16(addr uint16) uint16 {
	return uint16(c.read8(addr)) | uint16(c.read8(addr+1))<<8
}

func (c *CPU) write8(addr uint16, data uint8) {
	c.mem.Write8(addr, data)
}

func (c *CPU) write16(addr uint16, data uint16) {
	c.write8(addr, uint8(data))
	c.write8(addr+1, uint8(data>>8))
}

func (c CPU) getFlag(flag uint8) bool {
	return c.p&flag > 0
}

func (c *CPU) setFlag(flag uint8, v bool) {
	if v {
		c.p |= flag
		return
	}
	c.p &= ^flag
}

func (c *CPU) setFlagsZN(value uint8) {
	c.setFlag(flagZBit, value == 0)
	c.setFlag(flagNBit, value&0x80 > 0)
}

func (c *CPU) stackPop8() uint8 {
	c.sp++
	return c.read8(stackStartAddr + uint16(c.sp))
}

func (c *CPU) stackPop16() uint16 {
	lo := uint16(c.stackPop8())
	hi := uint16(c.stackPop8())
	return lo | hi<<8
}

func (c *CPU) stackPush8(data uint8) {
	c.write8(stackStartAddr+uint16(c.sp), data)
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

	opcode := c.read8(c.pc)
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
	c.a = 0
	c.x = 0
	c.y = 0
	c.p = 0x00 | flagUBit
	c.sp = 0xfd
	c.pc = c.read16(0xfffc)
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
	c.stackPush8(c.p)
	c.pc = c.read16(0xfffe)
	c.cycles = 7
}

// non-maskable interrupt request signal
func (c *CPU) NMI() {
	c.stackPush16(c.pc)
	c.setFlag(flagBBit, false)
	c.setFlag(flagUBit, true)
	c.setFlag(flagIBit, true)
	c.stackPush8(c.p)
	c.pc = c.read16(0xfffa)
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
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeZP:
		c.operandAddr = uint16(c.read8(c.pc))
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeZPX:
		c.operandAddr = uint16(c.read8(c.pc) + c.x)
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeZPY:
		c.operandAddr = uint16(c.read8(c.pc) + c.y)
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeABS:
		c.operandAddr = c.read16(c.pc)
		c.pc += 2
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeABSX:
		baseAddr := c.read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.x)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = (baseAddr & 0xff00) != (c.operandAddr & 0xff00)
		return

	case addrModeABSY:
		baseAddr := c.read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.y)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = (baseAddr & 0xff00) != (c.operandAddr & 0xff00)
		return

	case addrModeIND:
		addr := c.read16(c.pc)
		c.pc += 2
		// simulate 6502 page boundary hardware bug
		// if low byte is 0x00FF, then high byte is 0x0000
		lo := addr
		hi := (lo & 0xff00) | (lo&0x00ff + 1)
		c.operandAddr = uint16(c.read8(lo)) | uint16(c.read8(hi))<<8
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeINDX:
		addr := uint16(c.read8(c.pc) + c.x)
		c.pc++
		lo := uint16(c.read8(addr & 0x00ff))
		hi := uint16(c.read8((addr + 1) & 0x00ff))
		c.operandAddr = lo | hi<<8
		c.operandValue = c.read8(c.operandAddr)
		return

	case addrModeINDY:
		addr := uint16(c.read8(c.pc))
		c.pc++
		lo := uint16(c.read8(addr))
		hi := uint16(c.read8((addr + 1) & 0x00ff))
		c.operandAddr = (lo | hi<<8) + uint16(c.y)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = (lo & 0xff00) != (hi & 0xff00)
		return

	case addrModeREL:
		c.operandAddr = uint16(c.read8(c.pc))
		c.pc++
		if c.operandAddr&0x80 > 0 {
			c.operandAddr |= 0xFF00 // add leading 1 bits to save the sign
		}
		return

	case addrModeACC:
		c.operandValue = c.a
		return

	case addrModeIMP:
		return

	}

	log.Fatalln("unreachable. addr mode must be always valid")
}

// returns true if the two values have the same sign, false otherwise
func isSameSign(a, b uint8) bool {
	return (a^b)&0x80 == 0
}

// Add with Carry
// A = A + M + C
//
// Flags Affected: C, Z, N, V
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) adc() {
	r16 := uint16(c.a) + uint16(c.operandValue)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16)
	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlagsZN(r8)
	c.setFlag(flagVBit, isSameSign(c.a, c.operandValue) && !isSameSign(c.a, r8))
	if c.pageCrossed {
		c.cycles++
	}
	c.a = r8
}

// Logical AND
// A = A & M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed
func (c *CPU) and() {
	c.a &= c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

// Arithmetic Shift Left
// C <- (A or M)7, (A or M) << 1
//
// Flags affected: C, Z, N
func (c *CPU) asl() {
	c.setFlag(flagCBit, c.operandValue&0x80 > 0)
	r8 := c.operandValue << 1
	c.setFlagsZN(r8)
	if c.addrMode == addrModeACC {
		c.a = r8
	} else {
		c.write8(c.operandAddr, r8)
	}
}

// common branch instruction
func (c *CPU) jmpIf(condition bool) {
	if !condition {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if addr&0xff00 != c.pc&0xff00 {
		c.cycles++ // different pages
	}
	c.pc = addr
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
	c.jmpIf(!c.getFlag(flagCBit))
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
	c.jmpIf(c.getFlag(flagCBit))
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
	c.jmpIf(c.getFlag(flagZBit))
}

// Bit Test
// A & M, N <- M7, V <- M6
//
// Flags affected: Z, N, V
func (c *CPU) bit() {
	m := c.a & c.operandValue
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
	c.jmpIf(c.getFlag(flagNBit))
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
	c.jmpIf(!c.getFlag(flagZBit))
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
	c.jmpIf(!c.getFlag(flagNBit))
}

// Force Interrupt
//
// Flags affected: B, U
func (c *CPU) brk() {
	c.pc++
	c.stackPush16(c.pc)
	c.setFlag(flagIBit, true)
	c.setFlag(flagBBit, true)
	c.stackPush8(c.p)
	c.setFlag(flagBBit, false)
	c.pc = c.read16(0xfffe)
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
	c.jmpIf(!c.getFlag(flagVBit))
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
	c.jmpIf(c.getFlag(flagVBit))
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
	r := c.a - c.operandValue
	c.setFlag(flagCBit, c.a >= c.operandValue)
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
	r := c.x - c.operandValue
	c.setFlag(flagCBit, c.x >= c.operandValue)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)
}

// Compare Y Register
// Y - M
//
// Flags affected: C, Z, N
func (c *CPU) cpy() {
	r := c.y - c.operandValue
	c.setFlag(flagCBit, c.y >= c.operandValue)
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
	c.write8(c.operandAddr, r)
}

// Decrement X Register
// X - 1
//
// Flags affected: Z, N
func (c *CPU) dex() {
	c.x--
	c.setFlag(flagZBit, c.x == 0)
	c.setFlag(flagNBit, c.x&0x80 > 0)
}

// Decrement Y Register
// Y - 1
//
// Flags affected: Z, N
func (c *CPU) dey() {
	c.y--
	c.setFlag(flagZBit, c.y == 0)
	c.setFlag(flagNBit, c.y&0x80 > 0)
}

// Exclusive OR
// A ^ M
//
// Flags affected: Z, N
//
// An additional cycle is needed if the page boundary is crossed.
func (c *CPU) eor() {
	c.a ^= c.operandValue
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
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
	c.write8(c.operandAddr, r)
}

// Increment X Register
// X + 1
//
// Flags affected: Z, N
func (c *CPU) inx() {
	c.x++
	c.setFlag(flagZBit, c.x == 0)
	c.setFlag(flagNBit, c.x&0x80 > 0)
}

// Increment Y Register
// Y + 1
//
// Flags affected: Z, N
func (c *CPU) iny() {
	c.y++
	c.setFlag(flagZBit, c.y == 0)
	c.setFlag(flagNBit, c.y&0x80 > 0)
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
	c.a = c.operandValue
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
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
	c.x = c.operandValue
	c.setFlag(flagZBit, c.x == 0)
	c.setFlag(flagNBit, c.x&0x80 > 0)
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
	c.y = c.operandValue
	c.setFlag(flagZBit, c.y == 0)
	c.setFlag(flagNBit, c.y&0x80 > 0)
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
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
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
	c.a |= c.operandValue
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
	if c.pageCrossed {
		c.cycles++
	}
}

// Push Accumulator
// A -> stack
//
// Flags affected: None
func (c *CPU) pha() {
	c.stackPush8(c.a)
}

// Push Processor p
// p -> stack
func (c *CPU) php() {
	c.stackPush8(c.p)
}

// Pull Accumulator
// A <- stack
//
// Flags affected: Z, N
func (c *CPU) pla() {
	c.a = c.stackPop8()
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
}

// Pull Processor p
// p <- stack
//
// Flags affected: All
func (c *CPU) plp() {
	c.p = c.stackPop8()
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
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
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
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
	}
}

// Return from Interrupt
// p <- stack, PC <- stack
//
// Flags affected: All
func (c *CPU) rti() {
	c.p = c.stackPop8()
	c.p &= ^flagBBit
	c.p &= ^flagUBit
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
	r16 := uint16(c.a) + uint16(^c.operandValue)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16)
	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 > 0)
	c.setFlag(flagVBit, (r8^c.a)&(r8^c.operandValue)&0x80 > 0)
	c.a = r8
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
	c.write8(c.operandAddr, c.a)
}

// Store X Register
// M <- X
//
// Flags affected: None
func (c *CPU) stx() {
	c.write8(c.operandAddr, c.x)
}

// Store Y Register
// M <- Y
//
// Flags affected: None
func (c *CPU) sty() {
	c.write8(c.operandAddr, c.y)
}

// Transfer Accumulator to X
// X <- A
//
// Flags affected: Z, N
func (c *CPU) tax() {
	c.x = c.a
	c.setFlag(flagZBit, c.x == 0)
	c.setFlag(flagNBit, c.x&0x80 > 0)
}

// Transfer Accumulator to Y
// Y <- A
//
// Flags affected: Z, N
func (c *CPU) tay() {
	c.y = c.a
	c.setFlag(flagZBit, c.y == 0)
	c.setFlag(flagNBit, c.y&0x80 > 0)
}

// Transfer Stack Pointer to X
// X <- SP
//
// Flags affected: Z, N
func (c *CPU) tsx() {
	c.x = c.sp
	c.setFlag(flagZBit, c.x == 0)
	c.setFlag(flagNBit, c.x&0x80 > 0)
}

// Transfer X to Accumulator
// A <- X
//
// Flags affected: Z, N
func (c *CPU) txa() {
	c.a = c.x
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
}

// Transfer X to Stack Pointer
// SP <- X
//
// Flags affected: None
func (c *CPU) txs() {
	c.sp = c.x
}

// Transfer Y to Accumulator
// A <- Y
//
// Flags affected: Z, N
func (c *CPU) tya() {
	c.a = c.y
	c.setFlag(flagZBit, c.a == 0)
	c.setFlag(flagNBit, c.a&0x80 > 0)
}

func (c *CPU) initInstructions() {
	c.instrs[0x00] = instruction{name: "BRK", mode: addrModeIMP, cycles: 7, fn: c.brk}
	c.instrs[0x01] = instruction{name: "ORA", mode: addrModeINDX, cycles: 6, fn: c.ora}
	c.instrs[0x05] = instruction{name: "ORA", mode: addrModeZP, cycles: 3, fn: c.ora}
	c.instrs[0x06] = instruction{name: "ASL", mode: addrModeZP, cycles: 5, fn: c.asl}
	c.instrs[0x08] = instruction{name: "PHP", mode: addrModeIMP, cycles: 3, fn: c.php}
	c.instrs[0x09] = instruction{name: "ORA", mode: addrModeIMM, cycles: 2, fn: c.ora}
	c.instrs[0x0A] = instruction{name: "ASL", mode: addrModeACC, cycles: 2, fn: c.asl}
	c.instrs[0x0D] = instruction{name: "ORA", mode: addrModeABS, cycles: 4, fn: c.ora}
	c.instrs[0x0E] = instruction{name: "ASL", mode: addrModeABS, cycles: 6, fn: c.asl}
	c.instrs[0x10] = instruction{name: "BPL", mode: addrModeREL, cycles: 2, fn: c.bpl}
	c.instrs[0x11] = instruction{name: "ORA", mode: addrModeINDY, cycles: 5, fn: c.ora}
	c.instrs[0x15] = instruction{name: "ORA", mode: addrModeZPX, cycles: 4, fn: c.ora}
	c.instrs[0x16] = instruction{name: "ASL", mode: addrModeZPX, cycles: 6, fn: c.asl}
	c.instrs[0x18] = instruction{name: "CLC", mode: addrModeIMP, cycles: 2, fn: c.clc}
	c.instrs[0x19] = instruction{name: "ORA", mode: addrModeABSY, cycles: 4, fn: c.ora}
	c.instrs[0x1D] = instruction{name: "ORA", mode: addrModeABSX, cycles: 4, fn: c.ora}
	c.instrs[0x1E] = instruction{name: "ASL", mode: addrModeABSX, cycles: 7, fn: c.asl}
	c.instrs[0x20] = instruction{name: "JSR", mode: addrModeABS, cycles: 6, fn: c.jsr}
	c.instrs[0x21] = instruction{name: "AND", mode: addrModeINDX, cycles: 6, fn: c.and}
	c.instrs[0x24] = instruction{name: "BIT", mode: addrModeZP, cycles: 3, fn: c.bit}
	c.instrs[0x25] = instruction{name: "AND", mode: addrModeZP, cycles: 3, fn: c.and}
	c.instrs[0x26] = instruction{name: "ROL", mode: addrModeZP, cycles: 5, fn: c.rol}
	c.instrs[0x28] = instruction{name: "PLP", mode: addrModeIMP, cycles: 4, fn: c.plp}
	c.instrs[0x29] = instruction{name: "AND", mode: addrModeIMM, cycles: 2, fn: c.and}
	c.instrs[0x2A] = instruction{name: "ROL", mode: addrModeACC, cycles: 2, fn: c.rol}
	c.instrs[0x2C] = instruction{name: "BIT", mode: addrModeABS, cycles: 4, fn: c.bit}
	c.instrs[0x2D] = instruction{name: "AND", mode: addrModeABS, cycles: 4, fn: c.and}
	c.instrs[0x2E] = instruction{name: "ROL", mode: addrModeABS, cycles: 6, fn: c.rol}
	c.instrs[0x30] = instruction{name: "BMI", mode: addrModeREL, cycles: 2, fn: c.bmi}
	c.instrs[0x31] = instruction{name: "AND", mode: addrModeINDY, cycles: 5, fn: c.and}
	c.instrs[0x35] = instruction{name: "AND", mode: addrModeZPX, cycles: 4, fn: c.and}
	c.instrs[0x36] = instruction{name: "ROL", mode: addrModeZPX, cycles: 6, fn: c.rol}
	c.instrs[0x38] = instruction{name: "SEC", mode: addrModeIMP, cycles: 2, fn: c.sec}
	c.instrs[0x39] = instruction{name: "AND", mode: addrModeABSY, cycles: 4, fn: c.and}
	c.instrs[0x3D] = instruction{name: "AND", mode: addrModeABSX, cycles: 4, fn: c.and}
	c.instrs[0x3E] = instruction{name: "ROL", mode: addrModeABSX, cycles: 7, fn: c.rol}
	c.instrs[0x40] = instruction{name: "RTI", mode: addrModeIMP, cycles: 6, fn: c.rti}
	c.instrs[0x41] = instruction{name: "EOR", mode: addrModeINDX, cycles: 6, fn: c.eor}
	c.instrs[0x45] = instruction{name: "EOR", mode: addrModeZP, cycles: 3, fn: c.eor}
	c.instrs[0x46] = instruction{name: "LSR", mode: addrModeZP, cycles: 5, fn: c.lsr}
	c.instrs[0x48] = instruction{name: "PHA", mode: addrModeIMP, cycles: 3, fn: c.pha}
	c.instrs[0x49] = instruction{name: "EOR", mode: addrModeIMM, cycles: 2, fn: c.eor}
	c.instrs[0x4A] = instruction{name: "LSR", mode: addrModeACC, cycles: 2, fn: c.lsr}
	c.instrs[0x4C] = instruction{name: "JMP", mode: addrModeABS, cycles: 3, fn: c.jmp}
	c.instrs[0x4D] = instruction{name: "EOR", mode: addrModeABS, cycles: 4, fn: c.eor}
	c.instrs[0x4E] = instruction{name: "LSR", mode: addrModeABS, cycles: 6, fn: c.lsr}
	c.instrs[0x50] = instruction{name: "BVC", mode: addrModeREL, cycles: 2, fn: c.bvc}
	c.instrs[0x51] = instruction{name: "EOR", mode: addrModeINDY, cycles: 5, fn: c.eor}
	c.instrs[0x55] = instruction{name: "EOR", mode: addrModeZPX, cycles: 4, fn: c.eor}
	c.instrs[0x56] = instruction{name: "LSR", mode: addrModeZPX, cycles: 6, fn: c.lsr}
	c.instrs[0x58] = instruction{name: "CLI", mode: addrModeIMP, cycles: 2, fn: c.cli}
	c.instrs[0x59] = instruction{name: "EOR", mode: addrModeABSY, cycles: 4, fn: c.eor}
	c.instrs[0x5D] = instruction{name: "EOR", mode: addrModeABSX, cycles: 4, fn: c.eor}
	c.instrs[0x5E] = instruction{name: "LSR", mode: addrModeABSX, cycles: 7, fn: c.lsr}
	c.instrs[0x60] = instruction{name: "RTS", mode: addrModeIMP, cycles: 6, fn: c.rts}
	c.instrs[0x61] = instruction{name: "ADC", mode: addrModeINDX, cycles: 6, fn: c.adc}
	c.instrs[0x65] = instruction{name: "ADC", mode: addrModeZP, cycles: 3, fn: c.adc}
	c.instrs[0x66] = instruction{name: "ROR", mode: addrModeZP, cycles: 5, fn: c.ror}
	c.instrs[0x68] = instruction{name: "PLA", mode: addrModeIMP, cycles: 4, fn: c.pla}
	c.instrs[0x69] = instruction{name: "ADC", mode: addrModeIMM, cycles: 2, fn: c.adc}
	c.instrs[0x6A] = instruction{name: "ROR", mode: addrModeACC, cycles: 2, fn: c.ror}
	c.instrs[0x6C] = instruction{name: "JMP", mode: addrModeIND, cycles: 5, fn: c.jmp}
	c.instrs[0x6D] = instruction{name: "ADC", mode: addrModeABS, cycles: 4, fn: c.adc}
	c.instrs[0x6E] = instruction{name: "ROR", mode: addrModeABS, cycles: 6, fn: c.ror}
	c.instrs[0x70] = instruction{name: "BVS", mode: addrModeREL, cycles: 2, fn: c.bvs}
	c.instrs[0x71] = instruction{name: "ADC", mode: addrModeINDY, cycles: 5, fn: c.adc}
	c.instrs[0x75] = instruction{name: "ADC", mode: addrModeZPX, cycles: 4, fn: c.adc}
	c.instrs[0x76] = instruction{name: "ROR", mode: addrModeZPX, cycles: 6, fn: c.ror}
	c.instrs[0x78] = instruction{name: "SEI", mode: addrModeIMP, cycles: 2, fn: c.sei}
	c.instrs[0x79] = instruction{name: "ADC", mode: addrModeABSY, cycles: 4, fn: c.adc}
	c.instrs[0x7D] = instruction{name: "ADC", mode: addrModeABSX, cycles: 4, fn: c.adc}
	c.instrs[0x7E] = instruction{name: "ROR", mode: addrModeABSX, cycles: 7, fn: c.ror}
	c.instrs[0x81] = instruction{name: "STA", mode: addrModeINDX, cycles: 6, fn: c.sta}
	c.instrs[0x84] = instruction{name: "STY", mode: addrModeZP, cycles: 3, fn: c.sty}
	c.instrs[0x85] = instruction{name: "STA", mode: addrModeZP, cycles: 3, fn: c.sta}
	c.instrs[0x86] = instruction{name: "STX", mode: addrModeZP, cycles: 3, fn: c.stx}
	c.instrs[0x88] = instruction{name: "DEY", mode: addrModeIMP, cycles: 2, fn: c.dey}
	c.instrs[0x8A] = instruction{name: "TXA", mode: addrModeIMP, cycles: 2, fn: c.txa}
	c.instrs[0x8C] = instruction{name: "STY", mode: addrModeABS, cycles: 4, fn: c.sty}
	c.instrs[0x8D] = instruction{name: "STA", mode: addrModeABS, cycles: 4, fn: c.sta}
	c.instrs[0x8E] = instruction{name: "STX", mode: addrModeABS, cycles: 4, fn: c.stx}
	c.instrs[0x90] = instruction{name: "BCC", mode: addrModeREL, cycles: 2, fn: c.bcc}
	c.instrs[0x91] = instruction{name: "STA", mode: addrModeINDY, cycles: 6, fn: c.sta}
	c.instrs[0x94] = instruction{name: "STY", mode: addrModeZPX, cycles: 4, fn: c.sty}
	c.instrs[0x95] = instruction{name: "STA", mode: addrModeZPX, cycles: 4, fn: c.sta}
	c.instrs[0x96] = instruction{name: "STX", mode: addrModeZPY, cycles: 4, fn: c.stx}
	c.instrs[0x98] = instruction{name: "TYA", mode: addrModeIMP, cycles: 2, fn: c.tya}
	c.instrs[0x99] = instruction{name: "STA", mode: addrModeABSY, cycles: 5, fn: c.sta}
	c.instrs[0x9A] = instruction{name: "TXS", mode: addrModeIMP, cycles: 2, fn: c.txs}
	c.instrs[0x9D] = instruction{name: "STA", mode: addrModeABSX, cycles: 5, fn: c.sta}
	c.instrs[0xA0] = instruction{name: "LDY", mode: addrModeIMM, cycles: 2, fn: c.ldy}
	c.instrs[0xA1] = instruction{name: "LDA", mode: addrModeINDX, cycles: 6, fn: c.lda}
	c.instrs[0xA2] = instruction{name: "LDX", mode: addrModeIMM, cycles: 2, fn: c.ldx}
	c.instrs[0xA4] = instruction{name: "LDY", mode: addrModeZP, cycles: 3, fn: c.ldy}
	c.instrs[0xA5] = instruction{name: "LDA", mode: addrModeZP, cycles: 3, fn: c.lda}
	c.instrs[0xA6] = instruction{name: "LDX", mode: addrModeZP, cycles: 3, fn: c.ldx}
	c.instrs[0xA8] = instruction{name: "TAY", mode: addrModeIMP, cycles: 2, fn: c.tay}
	c.instrs[0xA9] = instruction{name: "LDA", mode: addrModeIMM, cycles: 2, fn: c.lda}
	c.instrs[0xAA] = instruction{name: "TAX", mode: addrModeIMP, cycles: 2, fn: c.tax}
	c.instrs[0xAC] = instruction{name: "LDY", mode: addrModeABS, cycles: 4, fn: c.ldy}
	c.instrs[0xAD] = instruction{name: "LDA", mode: addrModeABS, cycles: 4, fn: c.lda}
	c.instrs[0xAE] = instruction{name: "LDX", mode: addrModeABS, cycles: 4, fn: c.ldx}
	c.instrs[0xB0] = instruction{name: "BCS", mode: addrModeREL, cycles: 2, fn: c.bcs}
	c.instrs[0xB1] = instruction{name: "LDA", mode: addrModeINDY, cycles: 5, fn: c.lda}
	c.instrs[0xB4] = instruction{name: "LDY", mode: addrModeZPX, cycles: 4, fn: c.ldy}
	c.instrs[0xB5] = instruction{name: "LDA", mode: addrModeZPX, cycles: 4, fn: c.lda}
	c.instrs[0xB6] = instruction{name: "LDX", mode: addrModeZPY, cycles: 4, fn: c.ldx}
	c.instrs[0xB8] = instruction{name: "CLV", mode: addrModeIMP, cycles: 2, fn: c.clv}
	c.instrs[0xB9] = instruction{name: "LDA", mode: addrModeABSY, cycles: 4, fn: c.lda}
	c.instrs[0xBA] = instruction{name: "TSX", mode: addrModeIMP, cycles: 2, fn: c.tsx}
	c.instrs[0xBC] = instruction{name: "LDY", mode: addrModeABSX, cycles: 4, fn: c.ldy}
	c.instrs[0xBD] = instruction{name: "LDA", mode: addrModeABSX, cycles: 4, fn: c.lda}
	c.instrs[0xBE] = instruction{name: "LDX", mode: addrModeABSY, cycles: 4, fn: c.ldx}
	c.instrs[0xC0] = instruction{name: "CPY", mode: addrModeIMM, cycles: 2, fn: c.cpy}
	c.instrs[0xC1] = instruction{name: "CMP", mode: addrModeINDX, cycles: 6, fn: c.cmp}
	c.instrs[0xC4] = instruction{name: "CPY", mode: addrModeZP, cycles: 3, fn: c.cpy}
	c.instrs[0xC5] = instruction{name: "CMP", mode: addrModeZP, cycles: 3, fn: c.cmp}
	c.instrs[0xC6] = instruction{name: "DEC", mode: addrModeZP, cycles: 5, fn: c.dec}
	c.instrs[0xC8] = instruction{name: "INY", mode: addrModeIMP, cycles: 2, fn: c.iny}
	c.instrs[0xC9] = instruction{name: "CMP", mode: addrModeIMM, cycles: 2, fn: c.cmp}
	c.instrs[0xCA] = instruction{name: "DEX", mode: addrModeIMP, cycles: 2, fn: c.dex}
	c.instrs[0xCC] = instruction{name: "CPY", mode: addrModeABS, cycles: 4, fn: c.cpy}
	c.instrs[0xCD] = instruction{name: "CMP", mode: addrModeABS, cycles: 4, fn: c.cmp}
	c.instrs[0xCE] = instruction{name: "DEC", mode: addrModeABS, cycles: 6, fn: c.dec}
	c.instrs[0xD0] = instruction{name: "BNE", mode: addrModeREL, cycles: 2, fn: c.bne}
	c.instrs[0xD1] = instruction{name: "CMP", mode: addrModeINDY, cycles: 5, fn: c.cmp}
	c.instrs[0xD5] = instruction{name: "CMP", mode: addrModeZPX, cycles: 4, fn: c.cmp}
	c.instrs[0xD6] = instruction{name: "DEC", mode: addrModeZPX, cycles: 6, fn: c.dec}
	c.instrs[0xD8] = instruction{name: "CLD", mode: addrModeIMP, cycles: 2, fn: c.cld}
	c.instrs[0xD9] = instruction{name: "CMP", mode: addrModeABSY, cycles: 4, fn: c.cmp}
	c.instrs[0xDD] = instruction{name: "CMP", mode: addrModeABSX, cycles: 4, fn: c.cmp}
	c.instrs[0xDE] = instruction{name: "DEC", mode: addrModeABSX, cycles: 7, fn: c.dec}
	c.instrs[0xE0] = instruction{name: "CPX", mode: addrModeIMM, cycles: 2, fn: c.cpx}
	c.instrs[0xE1] = instruction{name: "SBC", mode: addrModeINDX, cycles: 6, fn: c.sbc}
	c.instrs[0xE4] = instruction{name: "CPX", mode: addrModeZP, cycles: 3, fn: c.cpx}
	c.instrs[0xE5] = instruction{name: "SBC", mode: addrModeZP, cycles: 3, fn: c.sbc}
	c.instrs[0xE6] = instruction{name: "INC", mode: addrModeZP, cycles: 5, fn: c.inc}
	c.instrs[0xE8] = instruction{name: "INX", mode: addrModeIMP, cycles: 2, fn: c.inx}
	c.instrs[0xE9] = instruction{name: "SBC", mode: addrModeIMM, cycles: 2, fn: c.sbc}
	c.instrs[0xEA] = instruction{name: "NOP", mode: addrModeIMP, cycles: 2, fn: c.nop}
	c.instrs[0xEC] = instruction{name: "CPX", mode: addrModeABS, cycles: 4, fn: c.cpx}
	c.instrs[0xED] = instruction{name: "SBC", mode: addrModeABS, cycles: 4, fn: c.sbc}
	c.instrs[0xEE] = instruction{name: "INC", mode: addrModeABS, cycles: 6, fn: c.inc}
	c.instrs[0xF0] = instruction{name: "BEQ", mode: addrModeREL, cycles: 2, fn: c.beq}
	c.instrs[0xF1] = instruction{name: "SBC", mode: addrModeINDY, cycles: 5, fn: c.sbc}
	c.instrs[0xF5] = instruction{name: "SBC", mode: addrModeZPX, cycles: 4, fn: c.sbc}
	c.instrs[0xF6] = instruction{name: "INC", mode: addrModeZPX, cycles: 6, fn: c.inc}
	c.instrs[0xF8] = instruction{name: "SED", mode: addrModeIMP, cycles: 2, fn: c.sed}
	c.instrs[0xF9] = instruction{name: "SBC", mode: addrModeABSY, cycles: 4, fn: c.sbc}
	c.instrs[0xFD] = instruction{name: "SBC", mode: addrModeABSX, cycles: 4, fn: c.sbc}
	c.instrs[0xFE] = instruction{name: "INC", mode: addrModeABSX, cycles: 7, fn: c.inc}
}
