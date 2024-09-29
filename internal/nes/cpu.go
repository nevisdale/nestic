package nes

import (
	"fmt"
	"log"
)

const (
	stackStartAddr = uint16(0x100)
)

const (
	flagC = uint8(1 << iota) // Carry
	flagZ                    // Zero
	flagI                    // Interrupt Disable
	flagD                    // Decimal Mode
	flagB                    // Break Command
	flagU                    // Unused
	flagV                    // Overflow
	flagN                    // Negative
)

type addrMode uint8

const (
	addrModeIMM  addrMode = iota + 1 // Immediate
	addrModeZP                       // Zero Page
	addrModeZPX                      // Zero Page X
	addrModeZPY                      // Zero Page Y
	addrModeABS                      // Absolute
	addrModeABSX                     // Absolute X
	addrModeABSY                     // Absolute Y
	addrModeIND                      // Indirect
	addrModeINDX                     // Indirect X
	addrModeINDY                     // Indirect Y
	addrModeREL                      // Relative
	addrModeACC                      // Accumulator
	addrModeIMP                      // Implied
)

func (mode addrMode) String() string {
	switch mode {
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
	}
	return "???"
}

type instr struct {
	name   string
	mode   addrMode
	fn     func()
	cycles uint8
}

type CPU struct {
	a            uint8
	x            uint8
	y            uint8
	p            uint8
	sp           uint8
	pc           uint16
	mem          ReadWriter
	instrs       [0x100]instr
	cycles       uint8
	totalCycles  uint64
	addrMode     addrMode
	operandAddr  uint16
	operandValue uint8
	pageCrossed  bool
	halted       bool
}

func isSameSign(a, b uint8) bool {
	return (a^b)&0x80 == 0
}

func isDiffPage(a, b uint16) bool {
	return a&0xff00 != b&0xff00
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
	c.setFlag(flagZ, value == 0)
	c.setFlag(flagN, value&flagN > 0)
}

func (c *CPU) stackPop8() uint8 {
	c.sp++
	return c.read8(stackStartAddr | uint16(c.sp))
}

func (c *CPU) stackPop16() uint16 {
	lo := uint16(c.stackPop8())
	hi := uint16(c.stackPop8())
	return lo | hi<<8
}

func (c *CPU) stackPush8(data uint8) {
	c.write8(stackStartAddr|uint16(c.sp), data)
	c.sp--
}

func (c *CPU) stackPush16(data uint16) {
	lo := uint8(data & 0xff)
	hi := uint8(data >> 8)
	c.stackPush8(hi)
	c.stackPush8(lo)
}

// Reset the CPU to its initial state
func (c *CPU) Reset() {
	c.a = 0
	c.x = 0
	c.y = 0
	c.p = 0x00 | flagU | flagI
	c.sp = 0xfd
	c.pc = c.read16(0xfffc)
	c.cycles = 7
	c.totalCycles = 7
	c.halted = false
}

// Interrupt request signal
func (c *CPU) IRQ() {
	if c.getFlag(flagI) {
		return
	}

	c.stackPush16(c.pc)
	c.setFlag(flagB, false)
	c.setFlag(flagU|flagI, true)
	c.stackPush8(c.p)
	c.pc = c.read16(0xfffe)
	c.cycles += 7
}

// Non-maskable interrupt request signal
func (c *CPU) NMI() {
	c.stackPush16(c.pc)
	c.setFlag(flagB, false)
	c.setFlag(flagU|flagI, true)
	c.stackPush8(c.p)
	c.pc = c.read16(0xfffa)
	c.cycles += 7
}

// Disassemble returns a map of addresses and their corresponding instructions
// from 0x0000 to 0xffff
func (c *CPU) Disassemble() map[uint16]string {
	disasm := make(map[uint16]string, 0x10000)

	addr := uint32(0)
	for addr <= 0xFFFF {
		pc := uint16(addr)
		opcode := c.read8(pc)
		instr := c.instrs[opcode]
		if instr.fn == nil {
			disasm[pc] = fmt.Sprintf("$%04X: ???", pc)
			addr++
			continue
		}

		pc++
		skip := uint32(0)
		switch instr.mode {
		case addrModeIMM:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s #$%02X {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeZP:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%02X {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeZPX:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%02X,X {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeZPY:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%02X,Y {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeABS:
			operand := c.read16(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%04X {%s}", addr, instr.name, operand, instr.mode)
			skip = 2
		case addrModeABSX:
			operand := c.read16(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%04X,X {%s}", addr, instr.name, operand, instr.mode)
			skip = 2
		case addrModeABSY:
			operand := c.read16(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%04X,Y {%s}", addr, instr.name, operand, instr.mode)
			skip = 2
		case addrModeIND:
			operand := c.read16(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s ($%04X) {%s}", addr, instr.name, operand, instr.mode)
			skip = 2
		case addrModeINDX:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s ($%02X,X) {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeINDY:
			operand := c.read8(pc)
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s ($%02X),Y {%s}", addr, instr.name, operand, instr.mode)
			skip = 1
		case addrModeREL:
			operand := uint16(c.read8(pc))
			pc++
			if operand&0x80 > 0 {
				operand |= 0xff00 // add leading 1 s to save the sign
			}
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s $%02X {%s}", addr, instr.name, pc+operand, instr.mode)
			skip = 1
		case addrModeACC:
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s A {%s}", addr, instr.name, instr.mode)
		case addrModeIMP:
			disasm[uint16(addr)] = fmt.Sprintf("$%04X: %s {%s}", addr, instr.name, instr.mode)
		}

		addr = addr + 1 + skip
	}

	return disasm
}

// Tic executes one CPU cycle and
// returns the number of cycles left for the current operation
func (c *CPU) Tic() uint8 {
	if c.halted {
		return 0
	}

	if c.cycles > 0 {
		c.cycles--
		return c.cycles
	}

	opcode := c.read8(c.pc)
	c.pc++
	instr := c.instrs[opcode]
	if instr.fn == nil {
		c.hlt()
		log.Printf("unsupported opcode %02X. PC: %04X. halting...\n", opcode, c.pc)
		return 0
	}
	_ = c.fetch(instr.mode)
	instr.fn()
	c.cycles += instr.cycles
	c.totalCycles += uint64(c.cycles)

	c.addrMode = 0
	c.operandAddr = 0
	c.operandValue = 0
	c.pageCrossed = false
	return c.cycles
}

// fetch fetches the operand for the current instruction
// and returns the number of bytes read
func (c *CPU) fetch(addrMode addrMode) (n int) {
	c.addrMode = addrMode
	c.pageCrossed = false
	c.operandAddr = 0
	c.operandValue = 0

	switch addrMode {
	case addrModeIMM:
		c.operandAddr = c.pc
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return 1

	case addrModeZP:
		c.operandAddr = uint16(c.read8(c.pc))
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return 1

	case addrModeZPX:
		c.operandAddr = uint16(c.read8(c.pc) + c.x)
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return 1

	case addrModeZPY:
		c.operandAddr = uint16(c.read8(c.pc) + c.y)
		c.pc++
		c.operandValue = c.read8(c.operandAddr)
		return 1

	case addrModeABS:
		c.operandAddr = c.read16(c.pc)
		c.pc += 2
		c.operandValue = c.read8(c.operandAddr)
		return 2

	case addrModeABSX:
		baseAddr := c.read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.x)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = isDiffPage(baseAddr, c.operandAddr)
		return 2

	case addrModeABSY:
		baseAddr := c.read16(c.pc)
		c.pc += 2
		c.operandAddr = baseAddr + uint16(c.y)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = isDiffPage(baseAddr, c.operandAddr)
		return 2

	case addrModeIND:
		addr := c.read16(c.pc)
		c.pc += 2

		lo := addr
		hi := addr + 1
		if lo&0xff == 0xff { // simulate 6502 bug
			hi = (lo & 0xff00) | uint16((lo+1)&0x00ff)
		}
		c.operandAddr = uint16(c.read8(lo)) | uint16(c.read8(hi))<<8
		c.operandValue = c.read8(c.operandAddr)
		return 2

	case addrModeINDX:
		addr := uint16(c.read8(c.pc))
		addr = addr + uint16(c.x)
		c.pc++
		lo := uint16(c.read8(addr & 0x00ff))
		hi := uint16(c.read8((addr + 1) & 0x00ff))
		c.operandAddr = lo | hi<<8
		c.operandValue = c.read8(c.operandAddr)
		return 1

	case addrModeINDY:
		addr := uint16(c.read8(c.pc))
		c.pc++
		lo := uint16(c.read8(addr))
		hi := uint16(c.read8((addr + 1) & 0x00ff))
		addr = lo | hi<<8
		c.operandAddr = addr + uint16(c.y)
		c.operandValue = c.read8(c.operandAddr)
		c.pageCrossed = isDiffPage(addr, c.operandAddr)
		return 1

	case addrModeREL:
		c.operandAddr = uint16(c.read8(c.pc))
		c.pc++
		if c.operandAddr&0x80 > 0 {
			c.operandAddr |= 0xff00 // add leading 1 s to save the sign
		}
		return 1

	case addrModeACC:
		c.operandValue = c.a
		return 0

	case addrModeIMP:
		return 0
	}

	c.hlt()
	log.Printf("unsupported addressing mode %d. PC: %04X. halting...\n", addrMode, c.pc)
	return 0
}

func (c *CPU) adc() {
	r16 := uint16(c.a) + uint16(c.operandValue)
	if c.getFlag(flagC) {
		r16++
	}
	r8 := uint8(r16)
	c.setFlag(flagC, r16 > 0xff)
	c.setFlagsZN(r8)
	c.setFlag(flagV, isSameSign(c.a, c.operandValue) && !isSameSign(c.a, r8))
	c.a = r8
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) and() {
	c.a &= c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) asl() {
	c.setFlag(flagC, c.operandValue&0x80 > 0)
	r8 := c.operandValue << 1
	c.setFlagsZN(r8)
	if c.addrMode == addrModeACC {
		c.a = r8
	} else {
		c.write8(c.operandAddr, r8)
	}
}

func (c *CPU) jmpIf(condition bool) {
	if !condition {
		return
	}
	c.cycles++
	addr := c.pc + c.operandAddr
	if isDiffPage(c.pc, addr) {
		c.cycles++
	}
	c.pc = addr
}

func (c *CPU) bcc() {
	c.jmpIf(!c.getFlag(flagC))
}

func (c *CPU) bcs() {
	c.jmpIf(c.getFlag(flagC))
}

func (c *CPU) beq() {
	c.jmpIf(c.getFlag(flagZ))
}

func (c *CPU) bit() {
	m := c.a & c.operandValue
	c.setFlag(flagZ, m == 0)
	c.setFlag(flagN, c.operandValue&flagN > 0)
	c.setFlag(flagV, c.operandValue&flagV > 0)
}

func (c *CPU) bmi() {
	c.jmpIf(c.getFlag(flagN))
}

func (c *CPU) bne() {
	c.jmpIf(!c.getFlag(flagZ))
}

func (c *CPU) bpl() {
	c.jmpIf(!c.getFlag(flagN))
}

func (c *CPU) brk() {
	c.pc++
	c.stackPush16(c.pc)
	c.stackPush8(c.p | flagB)
	c.setFlag(flagI, true)
	c.pc = c.read16(0xfffe)
}

func (c *CPU) bvc() {
	c.jmpIf(!c.getFlag(flagV))
}

func (c *CPU) bvs() {
	c.jmpIf(c.getFlag(flagV))
}

func (c *CPU) clc() {
	c.setFlag(flagC, false)
}

func (c *CPU) cld() {
	c.setFlag(flagD, false)
}

func (c *CPU) cli() {
	c.setFlag(flagI, false)
}

func (c *CPU) clv() {
	c.setFlag(flagV, false)
}

func (c *CPU) cmp() {
	c.setFlag(flagC, c.a >= c.operandValue)
	c.setFlagsZN(c.a - c.operandValue)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) cpx() {
	c.setFlag(flagC, c.x >= c.operandValue)
	c.setFlagsZN(c.x - c.operandValue)
}

func (c *CPU) cpy() {
	c.setFlag(flagC, c.y >= c.operandValue)
	c.setFlagsZN(c.y - c.operandValue)
}

func (c *CPU) dec() {
	r := c.operandValue - 1
	c.setFlagsZN(r)
	c.write8(c.operandAddr, r)
}

func (c *CPU) dex() {
	c.x--
	c.setFlagsZN(c.x)
}

func (c *CPU) dey() {
	c.y--
	c.setFlagsZN(c.y)
}

func (c *CPU) eor() {
	c.a ^= c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) inc() {
	r := c.operandValue + 1
	c.setFlagsZN(r)
	c.write8(c.operandAddr, r)
}

func (c *CPU) inx() {
	c.x++
	c.setFlagsZN(c.x)
}

func (c *CPU) iny() {
	c.y++
	c.setFlagsZN(c.y)
}

func (c *CPU) jmp() {
	c.pc = c.operandAddr
}

func (c *CPU) jsr() {
	// pc incremented by 1 after the fetch,
	// so we need to decrement it
	c.pc--
	c.stackPush16(c.pc)
	c.pc = c.operandAddr
}

func (c *CPU) lda() {
	c.a = c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) ldx() {
	c.x = c.operandValue
	c.setFlagsZN(c.x)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) ldy() {
	c.y = c.operandValue
	c.setFlagsZN(c.y)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) lsr() {
	c.setFlag(flagC, c.operandValue&0x1 > 0)
	r := c.operandValue >> 1
	c.setFlagsZN(r)
	if c.addrMode == addrModeACC {
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
	}
}

func (c *CPU) nop() {
	// it needs for illegal opcodes
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) ora() {
	c.a |= c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) pha() {
	c.stackPush8(c.a)
}

func (c *CPU) php() {
	c.stackPush8(c.p | flagB)
}

func (c *CPU) pla() {
	c.a = c.stackPop8()
	c.setFlagsZN(c.a)
}

func (c *CPU) plp() {
	c.p = (c.stackPop8() | flagU) & ^flagB
}

func (c *CPU) rol() {
	r := c.operandValue << 1
	if c.getFlag(flagC) {
		r |= 0x1
	}
	c.setFlag(flagC, c.operandValue&0x80 > 0)
	c.setFlagsZN(r)
	if c.addrMode == addrModeACC {
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
	}
}

func (c *CPU) ror() {
	r := c.operandValue >> 1
	if c.getFlag(flagC) {
		r |= 0x80
	}
	c.setFlag(flagC, c.operandValue&0x1 > 0)
	c.setFlagsZN(r)
	if c.addrMode == addrModeACC {
		c.a = r
	} else {
		c.write8(c.operandAddr, r)
	}
}

func (c *CPU) rti() {
	c.p = (c.stackPop8() | flagU) & ^flagB
	c.pc = c.stackPop16()
}

func (c *CPU) rts() {
	c.pc = c.stackPop16()
	c.pc++
}

func (c *CPU) sbc() {
	c.operandValue = ^c.operandValue
	c.adc()
}

func (c *CPU) sec() {
	c.setFlag(flagC, true)
}

func (c *CPU) sed() {
	c.setFlag(flagD, true)
}

func (c *CPU) sei() {
	c.setFlag(flagI, true)
}

func (c *CPU) sta() {
	c.write8(c.operandAddr, c.a)
}

func (c *CPU) stx() {
	c.write8(c.operandAddr, c.x)
}

func (c *CPU) sty() {
	c.write8(c.operandAddr, c.y)
}

func (c *CPU) tax() {
	c.x = c.a
	c.setFlagsZN(c.x)
}

func (c *CPU) tay() {
	c.y = c.a
	c.setFlagsZN(c.y)
}

func (c *CPU) tsx() {
	c.x = c.sp
	c.setFlagsZN(c.x)
}

func (c *CPU) txa() {
	c.a = c.x
	c.setFlagsZN(c.a)
}

func (c *CPU) txs() {
	c.sp = c.x
}

func (c *CPU) tya() {
	c.a = c.y
	c.setFlagsZN(c.a)
}

func (c *CPU) lax() {
	c.a = c.operandValue
	c.x = c.operandValue
	c.setFlagsZN(c.a)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) sax() {
	c.write8(c.operandAddr, c.a&c.x)
}

func (c *CPU) dcp() {
	c.operandValue--
	c.write8(c.operandAddr, c.operandValue)
	c.pageCrossed = false
	c.cmp()
}

func (c *CPU) isc() {
	c.operandValue++
	c.write8(c.operandAddr, c.operandValue)
	c.pageCrossed = false
	c.sbc()
}

func (c *CPU) slo() {
	c.setFlag(flagC, c.operandValue&0x80 > 0)
	r := c.operandValue << 1
	c.write8(c.operandAddr, r)
	c.a |= r
	c.setFlagsZN(c.a)
}

func (c *CPU) rla() {
	carry := c.operandValue&0x80 > 0
	r := c.operandValue << 1
	if c.getFlag(flagC) {
		r |= 0x1
	}
	c.write8(c.operandAddr, r)
	c.a &= r
	c.setFlag(flagC, carry)
	c.setFlagsZN(c.a)
}

func (c *CPU) sre() {
	c.setFlag(flagC, c.operandValue&0x1 > 0)
	r := c.operandValue >> 1
	c.write8(c.operandAddr, r)
	c.a ^= r
	c.setFlagsZN(c.a)
}

func (c *CPU) rra() {
	r := c.operandValue >> 1
	if c.getFlag(flagC) {
		r |= 0x80
	}
	c.setFlag(flagC, c.operandValue&0x1 > 0)
	c.operandValue = r
	c.write8(c.operandAddr, c.operandValue)
	c.pageCrossed = false
	c.adc()
}

func (c *CPU) hlt() {
	c.halted = true
}

func (c *CPU) anc() {
	c.a &= c.operandValue
	c.setFlag(flagC, c.a&0x80 > 0)
	c.setFlagsZN(c.a)
}

func (c *CPU) alr() {
	c.a &= c.operandValue
	c.setFlag(flagC, c.a&0x1 > 0)
	c.a >>= 1
	c.setFlagsZN(c.a)
}

func (c *CPU) las() {
	r := c.operandValue & c.sp
	c.a = r
	c.x = r
	c.sp = r
	c.setFlagsZN(r)
	if c.pageCrossed {
		c.cycles++
	}
}

func (c *CPU) initInstructions() {
	c.instrs[0x00] = instr{name: "BRK", mode: addrModeIMP, fn: c.brk, cycles: 7}
	c.instrs[0x01] = instr{name: "ORA", mode: addrModeINDX, fn: c.ora, cycles: 6}
	c.instrs[0x02] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x03] = instr{name: "SLO", mode: addrModeINDX, fn: c.slo, cycles: 8}
	c.instrs[0x04] = instr{name: "NOP", mode: addrModeZP, fn: c.nop, cycles: 3}
	c.instrs[0x05] = instr{name: "ORA", mode: addrModeZP, fn: c.ora, cycles: 3}
	c.instrs[0x06] = instr{name: "ASL", mode: addrModeZP, fn: c.asl, cycles: 5}
	c.instrs[0x07] = instr{name: "SLO", mode: addrModeZP, fn: c.slo, cycles: 5}
	c.instrs[0x08] = instr{name: "PHP", mode: addrModeIMP, fn: c.php, cycles: 3}
	c.instrs[0x09] = instr{name: "ORA", mode: addrModeIMM, fn: c.ora, cycles: 2}
	c.instrs[0x0a] = instr{name: "ASL", mode: addrModeACC, fn: c.asl, cycles: 2}
	c.instrs[0x0b] = instr{name: "ANC", mode: addrModeIMM, fn: c.anc, cycles: 2}
	c.instrs[0x0c] = instr{name: "NOP", mode: addrModeABS, fn: c.nop, cycles: 4}
	c.instrs[0x0d] = instr{name: "ORA", mode: addrModeABS, fn: c.ora, cycles: 4}
	c.instrs[0x0e] = instr{name: "ASL", mode: addrModeABS, fn: c.asl, cycles: 6}
	c.instrs[0x0f] = instr{name: "SLO", mode: addrModeABS, fn: c.slo, cycles: 6}
	c.instrs[0x10] = instr{name: "BPL", mode: addrModeREL, fn: c.bpl, cycles: 2}
	c.instrs[0x11] = instr{name: "ORA", mode: addrModeINDY, fn: c.ora, cycles: 5}
	c.instrs[0x12] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x13] = instr{name: "SLO", mode: addrModeINDY, fn: c.slo, cycles: 8}
	c.instrs[0x14] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0x15] = instr{name: "ORA", mode: addrModeZPX, fn: c.ora, cycles: 4}
	c.instrs[0x16] = instr{name: "ASL", mode: addrModeZPX, fn: c.asl, cycles: 6}
	c.instrs[0x17] = instr{name: "SLO", mode: addrModeZPX, fn: c.slo, cycles: 6}
	c.instrs[0x18] = instr{name: "CLC", mode: addrModeIMP, fn: c.clc, cycles: 2}
	c.instrs[0x19] = instr{name: "ORA", mode: addrModeABSY, fn: c.ora, cycles: 4}
	c.instrs[0x1a] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0x1b] = instr{name: "SLO", mode: addrModeABSY, fn: c.slo, cycles: 7}
	c.instrs[0x1c] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0x1d] = instr{name: "ORA", mode: addrModeABSX, fn: c.ora, cycles: 4}
	c.instrs[0x1e] = instr{name: "ASL", mode: addrModeABSX, fn: c.asl, cycles: 7}
	c.instrs[0x1f] = instr{name: "SLO", mode: addrModeABSX, fn: c.slo, cycles: 7}
	c.instrs[0x20] = instr{name: "JSR", mode: addrModeABS, fn: c.jsr, cycles: 6}
	c.instrs[0x21] = instr{name: "AND", mode: addrModeINDX, fn: c.and, cycles: 6}
	c.instrs[0x22] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x23] = instr{name: "RLA", mode: addrModeINDX, fn: c.rla, cycles: 8}
	c.instrs[0x24] = instr{name: "BIT", mode: addrModeZP, fn: c.bit, cycles: 3}
	c.instrs[0x25] = instr{name: "AND", mode: addrModeZP, fn: c.and, cycles: 3}
	c.instrs[0x26] = instr{name: "ROL", mode: addrModeZP, fn: c.rol, cycles: 5}
	c.instrs[0x27] = instr{name: "RLA", mode: addrModeZP, fn: c.rla, cycles: 5}
	c.instrs[0x28] = instr{name: "PLP", mode: addrModeIMP, fn: c.plp, cycles: 4}
	c.instrs[0x29] = instr{name: "AND", mode: addrModeIMM, fn: c.and, cycles: 2}
	c.instrs[0x2a] = instr{name: "ROL", mode: addrModeACC, fn: c.rol, cycles: 2}
	c.instrs[0x2b] = instr{name: "ANC", mode: addrModeIMM, fn: c.anc, cycles: 2}
	c.instrs[0x2c] = instr{name: "BIT", mode: addrModeABS, fn: c.bit, cycles: 4}
	c.instrs[0x2d] = instr{name: "AND", mode: addrModeABS, fn: c.and, cycles: 4}
	c.instrs[0x2e] = instr{name: "ROL", mode: addrModeABS, fn: c.rol, cycles: 6}
	c.instrs[0x2f] = instr{name: "RLA", mode: addrModeABS, fn: c.rla, cycles: 6}
	c.instrs[0x30] = instr{name: "BMI", mode: addrModeREL, fn: c.bmi, cycles: 2}
	c.instrs[0x31] = instr{name: "AND", mode: addrModeINDY, fn: c.and, cycles: 5}
	c.instrs[0x32] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x33] = instr{name: "RLA", mode: addrModeINDY, fn: c.rla, cycles: 8}
	c.instrs[0x34] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0x35] = instr{name: "AND", mode: addrModeZPX, fn: c.and, cycles: 4}
	c.instrs[0x36] = instr{name: "ROL", mode: addrModeZPX, fn: c.rol, cycles: 6}
	c.instrs[0x37] = instr{name: "RLA", mode: addrModeZPX, fn: c.rla, cycles: 6}
	c.instrs[0x38] = instr{name: "SEC", mode: addrModeIMP, fn: c.sec, cycles: 2}
	c.instrs[0x39] = instr{name: "AND", mode: addrModeABSY, fn: c.and, cycles: 4}
	c.instrs[0x3a] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0x3b] = instr{name: "RLA", mode: addrModeABSY, fn: c.rla, cycles: 7}
	c.instrs[0x3c] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0x3d] = instr{name: "AND", mode: addrModeABSX, fn: c.and, cycles: 4}
	c.instrs[0x3e] = instr{name: "ROL", mode: addrModeABSX, fn: c.rol, cycles: 7}
	c.instrs[0x3f] = instr{name: "RLA", mode: addrModeABSX, fn: c.rla, cycles: 7}
	c.instrs[0x40] = instr{name: "RTI", mode: addrModeIMP, fn: c.rti, cycles: 6}
	c.instrs[0x41] = instr{name: "EOR", mode: addrModeINDX, fn: c.eor, cycles: 6}
	c.instrs[0x42] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x43] = instr{name: "SRE", mode: addrModeINDX, fn: c.sre, cycles: 8}
	c.instrs[0x44] = instr{name: "NOP", mode: addrModeZP, fn: c.nop, cycles: 3}
	c.instrs[0x45] = instr{name: "EOR", mode: addrModeZP, fn: c.eor, cycles: 3}
	c.instrs[0x46] = instr{name: "LSR", mode: addrModeZP, fn: c.lsr, cycles: 5}
	c.instrs[0x47] = instr{name: "SRE", mode: addrModeZP, fn: c.sre, cycles: 5}
	c.instrs[0x48] = instr{name: "PHA", mode: addrModeIMP, fn: c.pha, cycles: 3}
	c.instrs[0x49] = instr{name: "EOR", mode: addrModeIMM, fn: c.eor, cycles: 2}
	c.instrs[0x4a] = instr{name: "LSR", mode: addrModeACC, fn: c.lsr, cycles: 2}
	c.instrs[0x4b] = instr{name: "ALR", mode: addrModeIMM, fn: c.alr, cycles: 2}
	c.instrs[0x4c] = instr{name: "JMP", mode: addrModeABS, fn: c.jmp, cycles: 3}
	c.instrs[0x4d] = instr{name: "EOR", mode: addrModeABS, fn: c.eor, cycles: 4}
	c.instrs[0x4e] = instr{name: "LSR", mode: addrModeABS, fn: c.lsr, cycles: 6}
	c.instrs[0x4f] = instr{name: "SRE", mode: addrModeABS, fn: c.sre, cycles: 6}
	c.instrs[0x50] = instr{name: "BVC", mode: addrModeREL, fn: c.bvc, cycles: 2}
	c.instrs[0x51] = instr{name: "EOR", mode: addrModeINDY, fn: c.eor, cycles: 5}
	c.instrs[0x52] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x53] = instr{name: "SRE", mode: addrModeINDY, fn: c.sre, cycles: 8}
	c.instrs[0x54] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0x55] = instr{name: "EOR", mode: addrModeZPX, fn: c.eor, cycles: 4}
	c.instrs[0x56] = instr{name: "LSR", mode: addrModeZPX, fn: c.lsr, cycles: 6}
	c.instrs[0x57] = instr{name: "SRE", mode: addrModeZPX, fn: c.sre, cycles: 6}
	c.instrs[0x58] = instr{name: "CLI", mode: addrModeIMP, fn: c.cli, cycles: 2}
	c.instrs[0x59] = instr{name: "EOR", mode: addrModeABSY, fn: c.eor, cycles: 4}
	c.instrs[0x5a] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0x5b] = instr{name: "SRE", mode: addrModeABSY, fn: c.sre, cycles: 7}
	c.instrs[0x5c] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0x5d] = instr{name: "EOR", mode: addrModeABSX, fn: c.eor, cycles: 4}
	c.instrs[0x5e] = instr{name: "LSR", mode: addrModeABSX, fn: c.lsr, cycles: 7}
	c.instrs[0x5f] = instr{name: "SRE", mode: addrModeABSX, fn: c.sre, cycles: 7}
	c.instrs[0x60] = instr{name: "RTS", mode: addrModeIMP, fn: c.rts, cycles: 6}
	c.instrs[0x61] = instr{name: "ADC", mode: addrModeINDX, fn: c.adc, cycles: 6}
	c.instrs[0x62] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x63] = instr{name: "RRA", mode: addrModeINDX, fn: c.rra, cycles: 8}
	c.instrs[0x64] = instr{name: "NOP", mode: addrModeZP, fn: c.nop, cycles: 3}
	c.instrs[0x65] = instr{name: "ADC", mode: addrModeZP, fn: c.adc, cycles: 3}
	c.instrs[0x66] = instr{name: "ROR", mode: addrModeZP, fn: c.ror, cycles: 5}
	c.instrs[0x67] = instr{name: "RRA", mode: addrModeZP, fn: c.rra, cycles: 5}
	c.instrs[0x68] = instr{name: "PLA", mode: addrModeIMP, fn: c.pla, cycles: 4}
	c.instrs[0x69] = instr{name: "ADC", mode: addrModeIMM, fn: c.adc, cycles: 2}
	c.instrs[0x6a] = instr{name: "ROR", mode: addrModeACC, fn: c.ror, cycles: 2}
	c.instrs[0x6c] = instr{name: "JMP", mode: addrModeIND, fn: c.jmp, cycles: 5}
	c.instrs[0x6d] = instr{name: "ADC", mode: addrModeABS, fn: c.adc, cycles: 4}
	c.instrs[0x6e] = instr{name: "ROR", mode: addrModeABS, fn: c.ror, cycles: 6}
	c.instrs[0x6f] = instr{name: "RRA", mode: addrModeABS, fn: c.rra, cycles: 6}
	c.instrs[0x70] = instr{name: "BVS", mode: addrModeREL, fn: c.bvs, cycles: 2}
	c.instrs[0x71] = instr{name: "ADC", mode: addrModeINDY, fn: c.adc, cycles: 5}
	c.instrs[0x72] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x73] = instr{name: "RRA", mode: addrModeINDY, fn: c.rra, cycles: 8}
	c.instrs[0x74] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0x75] = instr{name: "ADC", mode: addrModeZPX, fn: c.adc, cycles: 4}
	c.instrs[0x76] = instr{name: "ROR", mode: addrModeZPX, fn: c.ror, cycles: 6}
	c.instrs[0x77] = instr{name: "RRA", mode: addrModeZPX, fn: c.rra, cycles: 6}
	c.instrs[0x78] = instr{name: "SEI", mode: addrModeIMP, fn: c.sei, cycles: 2}
	c.instrs[0x79] = instr{name: "ADC", mode: addrModeABSY, fn: c.adc, cycles: 4}
	c.instrs[0x7a] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0x7b] = instr{name: "RRA", mode: addrModeABSY, fn: c.rra, cycles: 7}
	c.instrs[0x7c] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0x7d] = instr{name: "ADC", mode: addrModeABSX, fn: c.adc, cycles: 4}
	c.instrs[0x7e] = instr{name: "ROR", mode: addrModeABSX, fn: c.ror, cycles: 7}
	c.instrs[0x7f] = instr{name: "RRA", mode: addrModeABSX, fn: c.rra, cycles: 7}
	c.instrs[0x80] = instr{name: "NOP", mode: addrModeREL, fn: c.nop, cycles: 2}
	c.instrs[0x81] = instr{name: "STA", mode: addrModeINDX, fn: c.sta, cycles: 6}
	c.instrs[0x82] = instr{name: "NOP", mode: addrModeIMM, fn: c.nop, cycles: 2}
	c.instrs[0x83] = instr{name: "SAX", mode: addrModeINDX, fn: c.sax, cycles: 6}
	c.instrs[0x84] = instr{name: "STY", mode: addrModeZP, fn: c.sty, cycles: 3}
	c.instrs[0x85] = instr{name: "STA", mode: addrModeZP, fn: c.sta, cycles: 3}
	c.instrs[0x86] = instr{name: "STX", mode: addrModeZP, fn: c.stx, cycles: 3}
	c.instrs[0x87] = instr{name: "SAX", mode: addrModeZP, fn: c.sax, cycles: 3}
	c.instrs[0x88] = instr{name: "DEY", mode: addrModeIMP, fn: c.dey, cycles: 2}
	c.instrs[0x89] = instr{name: "NOP", mode: addrModeIMM, fn: c.nop, cycles: 2}
	c.instrs[0x8a] = instr{name: "TXA", mode: addrModeIMP, fn: c.txa, cycles: 2}
	c.instrs[0x8c] = instr{name: "STY", mode: addrModeABS, fn: c.sty, cycles: 4}
	c.instrs[0x8d] = instr{name: "STA", mode: addrModeABS, fn: c.sta, cycles: 4}
	c.instrs[0x8e] = instr{name: "STX", mode: addrModeABS, fn: c.stx, cycles: 4}
	c.instrs[0x8f] = instr{name: "SAX", mode: addrModeABS, fn: c.sax, cycles: 4}
	c.instrs[0x90] = instr{name: "BCC", mode: addrModeREL, fn: c.bcc, cycles: 2}
	c.instrs[0x91] = instr{name: "STA", mode: addrModeINDY, fn: c.sta, cycles: 6}
	c.instrs[0x92] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0x94] = instr{name: "STY", mode: addrModeZPX, fn: c.sty, cycles: 4}
	c.instrs[0x95] = instr{name: "STA", mode: addrModeZPX, fn: c.sta, cycles: 4}
	c.instrs[0x96] = instr{name: "STX", mode: addrModeZPY, fn: c.stx, cycles: 4}
	c.instrs[0x97] = instr{name: "SAX", mode: addrModeZPY, fn: c.sax, cycles: 4}
	c.instrs[0x98] = instr{name: "TYA", mode: addrModeIMP, fn: c.tya, cycles: 2}
	c.instrs[0x99] = instr{name: "STA", mode: addrModeABSY, fn: c.sta, cycles: 5}
	c.instrs[0x9a] = instr{name: "TXS", mode: addrModeIMP, fn: c.txs, cycles: 2}
	c.instrs[0x9d] = instr{name: "STA", mode: addrModeABSX, fn: c.sta, cycles: 5}
	c.instrs[0xa0] = instr{name: "LDY", mode: addrModeIMM, fn: c.ldy, cycles: 2}
	c.instrs[0xa1] = instr{name: "LDA", mode: addrModeINDX, fn: c.lda, cycles: 6}
	c.instrs[0xa2] = instr{name: "LDX", mode: addrModeIMM, fn: c.ldx, cycles: 2}
	c.instrs[0xa3] = instr{name: "LAX", mode: addrModeINDX, fn: c.lax, cycles: 6}
	c.instrs[0xa4] = instr{name: "LDY", mode: addrModeZP, fn: c.ldy, cycles: 3}
	c.instrs[0xa5] = instr{name: "LDA", mode: addrModeZP, fn: c.lda, cycles: 3}
	c.instrs[0xa6] = instr{name: "LDX", mode: addrModeZP, fn: c.ldx, cycles: 3}
	c.instrs[0xa7] = instr{name: "LAX", mode: addrModeZP, fn: c.lax, cycles: 3}
	c.instrs[0xa8] = instr{name: "TAY", mode: addrModeIMP, fn: c.tay, cycles: 2}
	c.instrs[0xa9] = instr{name: "LDA", mode: addrModeIMM, fn: c.lda, cycles: 2}
	c.instrs[0xaa] = instr{name: "TAX", mode: addrModeIMP, fn: c.tax, cycles: 2}
	c.instrs[0xac] = instr{name: "LDY", mode: addrModeABS, fn: c.ldy, cycles: 4}
	c.instrs[0xad] = instr{name: "LDA", mode: addrModeABS, fn: c.lda, cycles: 4}
	c.instrs[0xae] = instr{name: "LDX", mode: addrModeABS, fn: c.ldx, cycles: 4}
	c.instrs[0xaf] = instr{name: "LAX", mode: addrModeABS, fn: c.lax, cycles: 4}
	c.instrs[0xb0] = instr{name: "BCS", mode: addrModeREL, fn: c.bcs, cycles: 2}
	c.instrs[0xb1] = instr{name: "LDA", mode: addrModeINDY, fn: c.lda, cycles: 5}
	c.instrs[0xb2] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0xb3] = instr{name: "LAX", mode: addrModeINDY, fn: c.lax, cycles: 5}
	c.instrs[0xb4] = instr{name: "LDY", mode: addrModeZPX, fn: c.ldy, cycles: 4}
	c.instrs[0xb5] = instr{name: "LDA", mode: addrModeZPX, fn: c.lda, cycles: 4}
	c.instrs[0xb6] = instr{name: "LDX", mode: addrModeZPY, fn: c.ldx, cycles: 4}
	c.instrs[0xb7] = instr{name: "LAX", mode: addrModeZPY, fn: c.lax, cycles: 4}
	c.instrs[0xb8] = instr{name: "CLV", mode: addrModeIMP, fn: c.clv, cycles: 2}
	c.instrs[0xb9] = instr{name: "LDA", mode: addrModeABSY, fn: c.lda, cycles: 4}
	c.instrs[0xba] = instr{name: "TSX", mode: addrModeIMP, fn: c.tsx, cycles: 2}
	c.instrs[0xbb] = instr{name: "LAS", mode: addrModeABSY, fn: c.las, cycles: 4}
	c.instrs[0xbc] = instr{name: "LDY", mode: addrModeABSX, fn: c.ldy, cycles: 4}
	c.instrs[0xbd] = instr{name: "LDA", mode: addrModeABSX, fn: c.lda, cycles: 4}
	c.instrs[0xbe] = instr{name: "LDX", mode: addrModeABSY, fn: c.ldx, cycles: 4}
	c.instrs[0xbf] = instr{name: "LAX", mode: addrModeABSY, fn: c.lax, cycles: 4}
	c.instrs[0xc0] = instr{name: "CPY", mode: addrModeIMM, fn: c.cpy, cycles: 2}
	c.instrs[0xc1] = instr{name: "CMP", mode: addrModeINDX, fn: c.cmp, cycles: 6}
	c.instrs[0xc2] = instr{name: "NOP", mode: addrModeIMM, fn: c.nop, cycles: 2}
	c.instrs[0xc3] = instr{name: "DCP", mode: addrModeINDX, fn: c.dcp, cycles: 8}
	c.instrs[0xc4] = instr{name: "CPY", mode: addrModeZP, fn: c.cpy, cycles: 3}
	c.instrs[0xc5] = instr{name: "CMP", mode: addrModeZP, fn: c.cmp, cycles: 3}
	c.instrs[0xc6] = instr{name: "DEC", mode: addrModeZP, fn: c.dec, cycles: 5}
	c.instrs[0xc7] = instr{name: "DCP", mode: addrModeZP, fn: c.dcp, cycles: 5}
	c.instrs[0xc8] = instr{name: "INY", mode: addrModeIMP, fn: c.iny, cycles: 2}
	c.instrs[0xc9] = instr{name: "CMP", mode: addrModeIMM, fn: c.cmp, cycles: 2}
	c.instrs[0xca] = instr{name: "DEX", mode: addrModeIMP, fn: c.dex, cycles: 2}
	c.instrs[0xcc] = instr{name: "CPY", mode: addrModeABS, fn: c.cpy, cycles: 4}
	c.instrs[0xcd] = instr{name: "CMP", mode: addrModeABS, fn: c.cmp, cycles: 4}
	c.instrs[0xce] = instr{name: "DEC", mode: addrModeABS, fn: c.dec, cycles: 6}
	c.instrs[0xcf] = instr{name: "DCP", mode: addrModeABS, fn: c.dcp, cycles: 6}
	c.instrs[0xd0] = instr{name: "BNE", mode: addrModeREL, fn: c.bne, cycles: 2}
	c.instrs[0xd1] = instr{name: "CMP", mode: addrModeINDY, fn: c.cmp, cycles: 5}
	c.instrs[0xd2] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0xd3] = instr{name: "DCP", mode: addrModeINDY, fn: c.dcp, cycles: 8}
	c.instrs[0xd4] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0xd5] = instr{name: "CMP", mode: addrModeZPX, fn: c.cmp, cycles: 4}
	c.instrs[0xd6] = instr{name: "DEC", mode: addrModeZPX, fn: c.dec, cycles: 6}
	c.instrs[0xd7] = instr{name: "DCP", mode: addrModeZPX, fn: c.dcp, cycles: 6}
	c.instrs[0xd8] = instr{name: "CLD", mode: addrModeIMP, fn: c.cld, cycles: 2}
	c.instrs[0xd9] = instr{name: "CMP", mode: addrModeABSY, fn: c.cmp, cycles: 4}
	c.instrs[0xda] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0xdb] = instr{name: "DCP", mode: addrModeABSY, fn: c.dcp, cycles: 7}
	c.instrs[0xdc] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0xdd] = instr{name: "CMP", mode: addrModeABSX, fn: c.cmp, cycles: 4}
	c.instrs[0xde] = instr{name: "DEC", mode: addrModeABSX, fn: c.dec, cycles: 7}
	c.instrs[0xdf] = instr{name: "DCP", mode: addrModeABSX, fn: c.dcp, cycles: 7}
	c.instrs[0xe0] = instr{name: "CPX", mode: addrModeIMM, fn: c.cpx, cycles: 2}
	c.instrs[0xe1] = instr{name: "SBC", mode: addrModeINDX, fn: c.sbc, cycles: 6}
	c.instrs[0xe2] = instr{name: "NOP", mode: addrModeIMM, fn: c.nop, cycles: 2}
	c.instrs[0xe3] = instr{name: "ISC", mode: addrModeINDX, fn: c.isc, cycles: 8}
	c.instrs[0xe4] = instr{name: "CPX", mode: addrModeZP, fn: c.cpx, cycles: 3}
	c.instrs[0xe5] = instr{name: "SBC", mode: addrModeZP, fn: c.sbc, cycles: 3}
	c.instrs[0xe6] = instr{name: "INC", mode: addrModeZP, fn: c.inc, cycles: 5}
	c.instrs[0xe7] = instr{name: "ISC", mode: addrModeZP, fn: c.isc, cycles: 5}
	c.instrs[0xe8] = instr{name: "INX", mode: addrModeIMP, fn: c.inx, cycles: 2}
	c.instrs[0xe9] = instr{name: "SBC", mode: addrModeIMM, fn: c.sbc, cycles: 2}
	c.instrs[0xea] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0xeb] = instr{name: "SBC", mode: addrModeIMM, fn: c.sbc, cycles: 2}
	c.instrs[0xec] = instr{name: "CPX", mode: addrModeABS, fn: c.cpx, cycles: 4}
	c.instrs[0xed] = instr{name: "SBC", mode: addrModeABS, fn: c.sbc, cycles: 4}
	c.instrs[0xee] = instr{name: "INC", mode: addrModeABS, fn: c.inc, cycles: 6}
	c.instrs[0xef] = instr{name: "ISC", mode: addrModeABS, fn: c.isc, cycles: 6}
	c.instrs[0xf0] = instr{name: "BEQ", mode: addrModeREL, fn: c.beq, cycles: 2}
	c.instrs[0xf1] = instr{name: "SBC", mode: addrModeINDY, fn: c.sbc, cycles: 5}
	c.instrs[0xf2] = instr{name: "HLT", mode: addrModeIMP, fn: c.hlt, cycles: 0}
	c.instrs[0xf3] = instr{name: "ISC", mode: addrModeINDY, fn: c.isc, cycles: 8}
	c.instrs[0xf4] = instr{name: "NOP", mode: addrModeZPX, fn: c.nop, cycles: 4}
	c.instrs[0xf5] = instr{name: "SBC", mode: addrModeZPX, fn: c.sbc, cycles: 4}
	c.instrs[0xf6] = instr{name: "INC", mode: addrModeZPX, fn: c.inc, cycles: 6}
	c.instrs[0xf7] = instr{name: "ISC", mode: addrModeZPX, fn: c.isc, cycles: 6}
	c.instrs[0xf8] = instr{name: "SED", mode: addrModeIMP, fn: c.sed, cycles: 2}
	c.instrs[0xf9] = instr{name: "SBC", mode: addrModeABSY, fn: c.sbc, cycles: 4}
	c.instrs[0xfa] = instr{name: "NOP", mode: addrModeIMP, fn: c.nop, cycles: 2}
	c.instrs[0xfb] = instr{name: "ISC", mode: addrModeABSY, fn: c.isc, cycles: 7}
	c.instrs[0xfc] = instr{name: "NOP", mode: addrModeABSX, fn: c.nop, cycles: 4}
	c.instrs[0xfd] = instr{name: "SBC", mode: addrModeABSX, fn: c.sbc, cycles: 4}
	c.instrs[0xfe] = instr{name: "INC", mode: addrModeABSX, fn: c.inc, cycles: 7}
	c.instrs[0xff] = instr{name: "ISC", mode: addrModeABSX, fn: c.isc, cycles: 7}
}

func opcodeIsSupported(opcode byte) bool {
	fake := NewCPU(nil)
	return fake.instrs[opcode].fn != nil
}
