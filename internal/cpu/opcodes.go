package cpu

import (
	"fmt"
	"strings"
)

type opcodeFunc func() uint8

// Add with Carry
func (c *CPU) adc() uint8 {
	m8 := c.fetch()
	r16 := uint16(c.regA) + uint16(m8)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16 & 0x00ff)

	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 > 0)
	c.setFlag(flagVBit, (c.regA^r8) & ^(c.regA^m8) & 0x80 != 0)

	c.regA = r8

	// because we are using the fetch method, we need to add 1 cycl
	// if there is a page boundary
	return 1
}

// Logical AND
func (c *CPU) and() uint8 {
	operand := c.fetch()
	c.regA &= operand

	c.setFlag(flagNBit, c.regA&0x80 > 0)
	c.setFlag(flagZBit, c.regA == 0)

	// if the operation crosses a page boundary, we need to add 1 cycle
	// an addicional cycle will be returned from the address mode above
	return 1
}

// Arithmetic Shift Left
func (c *CPU) asl() uint8 {
	r8 := c.fetch()
	r16 := uint16(r8) << 1

	c.setFlag(flagCBit, r16 > 0xff)
	c.setFlag(flagZBit, r16&0x00ff == 0)
	c.setFlag(flagNBit, r16&0x80 > 0)

	if c.instructions[c.opcode].addrMode == addrModeACC || c.instructions[c.opcode].addrMode == addrModeIMP {
		c.regA = uint8(r16 & 0x00ff)
	} else {
		c.bus.Write8(c.addrAbs, uint8(r16&0x00ff))
	}
	return 0
}

// Branch if Carry Clear
func (c *CPU) bcc() uint8 {
	if !c.getFlag(flagCBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Branch if Carry Set
func (c *CPU) bcs() uint8 {
	// we need to increase cycles here, because
	// REL address mode does not increase cycles inside
	// it applies for all branch instructions
	if c.getFlag(flagCBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Branch if Equal
func (c *CPU) beq() uint8 {
	if c.getFlag(flagZBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Bit Test
func (c *CPU) bit() uint8 {
	m := c.fetch() & c.regA

	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0) // 1 << 7
	c.setFlag(flagVBit, m&0x40 > 0) // 1 << 6

	return 0
}

// Branch if Minus
func (c *CPU) bmi() uint8 {
	if c.getFlag(flagNBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Branch if Not Equal
func (c *CPU) bne() uint8 {
	if !c.getFlag(flagZBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Branch if Positive
func (c *CPU) bpl() uint8 {
	if !c.getFlag(flagNBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Force Interrupt
func (c *CPU) brk() uint8 { return 0 }

// Branch if Overflow Clear
func (c *CPU) bvc() uint8 {
	if !c.getFlag(flagVBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Branch if Overflow Set
func (c *CPU) bvs() uint8 {
	if c.getFlag(flagVBit) {
		c.cycles++
		c.addrAbs = c.pc + c.addrRel
		// different pages
		if c.addrAbs&0xFF00 != c.pc&0xFF00 {
			c.cycles++
		}
		c.pc = c.addrAbs
	}
	return 0
}

// Clear Carry Flag
func (c *CPU) clc() uint8 {
	c.setFlag(flagCBit, false)
	return 0
}

// Clear Decimal Mode
func (c *CPU) cld() uint8 {
	c.setFlag(flagDBit, false)
	return 0
}

// Clear Interrupt Disable
func (c *CPU) cli() uint8 {
	c.setFlag(flagIBit, false)
	return 0
}

// Clear Overflow Flag
func (c *CPU) clv() uint8 {
	c.setFlag(flagVBit, false)
	return 0
}

// Compare
func (c *CPU) cmp() uint8 {
	m := c.fetch()
	r := c.regA - m

	c.setFlag(flagCBit, c.regA >= m)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)

	// if the operation crosses a page boundary, we need to add 1 cycle
	return 1
}

// Compare X Register
func (c *CPU) cpx() uint8 {
	m := c.fetch()
	r := c.regX - m

	c.setFlag(flagCBit, c.regX >= m)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)

	return 0
}

// Compare Y Register
func (c *CPU) cpy() uint8 {
	m := c.fetch()
	r := c.regY - m

	c.setFlag(flagCBit, c.regY >= m)
	c.setFlag(flagZBit, r == 0)
	c.setFlag(flagNBit, r&0x80 > 0)

	return 0
}

// Decrement Memory
func (c *CPU) dec() uint8 {
	m := c.fetch()
	m -= 1

	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0)

	c.bus.Write8(c.addrAbs, m)

	return 0
}

// Decrement X Register
func (c *CPU) dex() uint8 {
	c.regX -= 1

	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)

	return 0
}

// Decrement Y Register
func (c *CPU) dey() uint8 {
	c.regY -= 1

	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)

	return 0
}

// Exclusive OR
func (c *CPU) eor() uint8 {
	m := c.fetch()
	c.regA = c.regA ^ m

	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)

	return 1
}

// Increment Memory
func (c *CPU) inc() uint8 {
	m := c.fetch()
	m += 1

	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0)

	c.bus.Write8(c.addrAbs, m)

	return 0
}

// Increment X Register
func (c *CPU) inx() uint8 {
	c.regX += 1

	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)

	return 0
}

// Increment Y Register
func (c *CPU) iny() uint8 {
	c.regY += 1

	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)

	return 0
}

// Jump
func (c *CPU) jmp() uint8 {
	c.pc = c.addrAbs
	return 0
}

// Jump to Subroutine
func (c *CPU) jsr() uint8 {
	c.pc -= 1
	c.bus.Write16(stackBase+uint16(c.sp), c.pc)
	c.sp -= 2
	c.pc = c.addrAbs

	return 0
}

// Load Accumulator
func (c *CPU) lda() uint8 {
	m := c.fetch()
	c.regA = m

	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)

	return 1
}

// Load X Register
func (c *CPU) ldx() uint8 {
	m := c.fetch()
	c.regX = m

	c.setFlag(flagZBit, c.regX == 0)
	c.setFlag(flagNBit, c.regX&0x80 > 0)

	return 1
}

// Load Y Register
func (c *CPU) ldy() uint8 {
	m := c.fetch()
	c.regY = m

	c.setFlag(flagZBit, c.regY == 0)
	c.setFlag(flagNBit, c.regY&0x80 > 0)

	return 1
}

// Logical Shift Right
func (c *CPU) lsr() uint8 {
	m := c.fetch()
	c.setFlag(flagCBit, m&0x1 == 1)
	m >>= 1

	c.setFlag(flagNBit, m == 0)
	c.setFlag(flagZBit, m&0x80 == 1)

	if am := c.instructions[c.opcode].addrMode; am == addrModeIMP || am == addrModeACC {
		c.regA = m
	} else {
		c.bus.Write8(c.addrAbs, m)
	}

	return 0
}

// No Operation
func (c *CPU) nop() uint8 {
	return 0
}

// Logical Inclusive OR
func (c *CPU) ora() uint8 {
	m := c.fetch()
	c.regA = c.regA | m

	c.setFlag(flagNBit, c.regA == 0)
	c.setFlag(flagZBit, c.regA&0x80 > 0)

	return 1
}

// Push Accumulator
func (c *CPU) pha() uint8 {
	c.bus.Write8(stackBase+uint16(c.sp), c.regA)
	c.sp--
	return 0
}

// Push Processor Status
func (c *CPU) php() uint8 {
	c.bus.Write8(stackBase+uint16(c.sp), c.status)
	c.sp--
	return 0
}

// Pull Accumulator
func (c *CPU) pla() uint8 {
	c.sp++
	c.regA = c.bus.Read8(stackBase + uint16(c.sp))
	c.setFlag(flagZBit, c.regA == 0)
	c.setFlag(flagNBit, c.regA&0x80 > 0)
	return 0
}

// Pull Processor Status
func (c *CPU) plp() uint8 {
	c.sp++
	c.status = c.bus.Read8(stackBase + uint16(c.sp))
	return 0

}

// Rotate Left
func (c *CPU) rol() uint8 {
	m := c.fetch()
	resultC := m&0x80 > 0
	m = m << 1
	if c.getFlag(flagCBit) {
		m |= 0x1
	}

	c.setFlag(flagCBit, resultC)
	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0)

	if am := c.instructions[c.opcode].addrMode; am == addrModeIMP || am == addrModeACC {
		c.regA = m
	} else {
		c.bus.Write8(c.addrAbs, m)
	}

	return 0
}

// Rotate Right
func (c *CPU) ror() uint8 {
	m := c.fetch()
	m >>= 1

	if c.getFlag(flagCBit) {
		m |= 0x80
	}

	c.setFlag(flagCBit, m&0x1 > 0)
	c.setFlag(flagZBit, m == 0)
	c.setFlag(flagNBit, m&0x80 > 0)

	if am := c.instructions[c.opcode].addrMode; am == addrModeIMP || am == addrModeACC {
		c.regA = m
	} else {
		c.bus.Write8(c.addrAbs, m)
	}

	return 0
}

// Return from Interrupt
func (c *CPU) rti() uint8 {
	c.sp++
	c.status = c.bus.Read8(stackBase + uint16(c.sp))
	c.status &= ^flagBBit
	c.status &= ^flagUBit

	c.sp++
	c.pc = c.bus.Read16(stackBase + uint16(c.sp))

	return 0
}

// Return from Subroutine
func (c *CPU) rts() uint8 {
	c.sp += 2
	c.pc = c.bus.Read16(c.addrAbs)
	c.pc++
	return 0
}

// Subtract with Carry
func (c *CPU) sbc() uint8 {
	// TODO: need to test this

	m8 := c.fetch()
	m8 = ^m8

	r16 := uint16(c.regA) + uint16(m8)
	if c.getFlag(flagCBit) {
		r16++
	}
	r8 := uint8(r16 & 0x00ff)

	c.setFlag(flagCBit, r16 <= 0xff)
	c.setFlag(flagZBit, r8 == 0)
	c.setFlag(flagNBit, r8&0x80 != 0)
	c.setFlag(flagVBit, (c.regA^r8)&(r8^m8)&0x80 != 0)

	c.regA = r8

	// because we are using the fetch method, we need to add 1 cycl
	// if there is a page boundary
	return 1
}

// Set Carry Flag
func (c *CPU) sec() uint8 {
	c.setFlag(flagCBit, true)
	return 0
}

// Set Decimal Flag
func (c *CPU) sed() uint8 {
	c.setFlag(flagDBit, true)
	return 0
}

// Set Interrupt Disable
func (c *CPU) sei() uint8 {
	c.setFlag(flagIBit, true)
	return 0
}

// Store Accumulator
func (c *CPU) sta() uint8 {
	c.bus.Write8(c.addrAbs, c.regA)
	return 0
}

// Store X Register
func (c *CPU) stx() uint8 {
	c.bus.Write8(c.addrAbs, c.regX)
	return 0
}

// Store Y Register
func (c *CPU) sty() uint8 {
	c.bus.Write8(c.addrAbs, c.regY)
	return 0
}

// Transfer Accumulator to X
func (c *CPU) tax() uint8 {
	c.regX = c.regA

	c.setFlag(flagNBit, c.regX&0x80 > 0)
	c.setFlag(flagZBit, c.regX == 0)

	return 0
}

// Transfer Accumulator to Y
func (c *CPU) tay() uint8 {
	c.regY = c.regA

	c.setFlag(flagNBit, c.regY&0x80 > 0)
	c.setFlag(flagZBit, c.regY == 0)

	return 0
}

// Transfer Stack Pointer to X
func (c *CPU) tsx() uint8 {
	c.regX = c.sp

	c.setFlag(flagNBit, c.regX&0x80 > 0)
	c.setFlag(flagZBit, c.regX == 0)

	return 0
}

// Transfer X to Accumulator
func (c *CPU) txa() uint8 {
	c.regA = c.regX

	c.setFlag(flagNBit, c.regA&0x80 > 0)
	c.setFlag(flagZBit, c.regA == 0)

	return 0
}

// Transfer X to Stack Pointer
func (c *CPU) txs() uint8 {
	c.sp = c.regX

	return 0
}

// Transfer Y to Accumulator
func (c *CPU) tya() uint8 {
	c.regA = c.regX

	c.setFlag(flagNBit, c.regA&0x80 > 0)
	c.setFlag(flagZBit, c.regA == 0)

	return 0
}

func (c *CPU) opcodeFuncFromMnemonic(mnemonic string) (opcodeFunc, error) {
	mnemonic = strings.ToUpper(mnemonic)
	switch mnemonic {
	case "ADC":
		return c.adc, nil
	case "AND":
		return c.and, nil
	case "ASL":
		return c.asl, nil
	case "BCC":
		return c.bcc, nil
	case "BCS":
		return c.bcs, nil
	case "BEQ":
		return c.beq, nil
	case "BIT":
		return c.bit, nil
	case "BMI":
		return c.bmi, nil
	case "BNE":
		return c.bne, nil
	case "BPL":
		return c.bpl, nil
	case "BRK":
		return c.brk, nil
	case "BVC":
		return c.bvc, nil
	case "BVS":
		return c.bvs, nil
	case "CLC":
		return c.clc, nil
	case "CLD":
		return c.cld, nil
	case "CLI":
		return c.cli, nil
	case "CLV":
		return c.clv, nil
	case "CMP":
		return c.cmp, nil
	case "CPX":
		return c.cpx, nil
	case "CPY":
		return c.cpy, nil
	case "DEC":
		return c.dec, nil
	case "DEX":
		return c.dex, nil
	case "DEY":
		return c.dey, nil
	case "EOR":
		return c.eor, nil
	case "INC":
		return c.inc, nil
	case "INX":
		return c.inx, nil
	case "INY":
		return c.iny, nil
	case "JMP":
		return c.jmp, nil
	case "JSR":
		return c.jsr, nil
	case "LDA":
		return c.lda, nil
	case "LDX":
		return c.ldx, nil
	case "LDY":
		return c.ldy, nil
	case "LSR":
		return c.lsr, nil
	case "NOP":
		return c.nop, nil
	case "ORA":
		return c.ora, nil
	case "PHA":
		return c.pha, nil
	case "PHP":
		return c.php, nil
	case "PLA":
		return c.pla, nil
	case "PLP":
		return c.plp, nil
	case "ROL":
		return c.rol, nil
	case "ROR":
		return c.ror, nil
	case "RTI":
		return c.rti, nil
	case "RTS":
		return c.rts, nil
	case "SBC":
		return c.sbc, nil
	case "SEC":
		return c.sec, nil
	case "SED":
		return c.sed, nil
	case "SEI":
		return c.sei, nil
	case "STA":
		return c.sta, nil
	case "STX":
		return c.stx, nil
	case "STY":
		return c.sty, nil
	case "TAX":
		return c.tax, nil
	case "TAY":
		return c.tay, nil
	case "TSX":
		return c.tsx, nil
	case "TXA":
		return c.txa, nil
	case "TXS":
		return c.txs, nil
	case "TYA":
		return c.tya, nil
	default:
		return nil, fmt.Errorf("unknown mnemonic: %s", mnemonic)
	}
}
