package cpu

import (
	"fmt"
	"strings"
)

type opcodeFunc func() uint8

// Add with Carry
func (c *CPU) adc() uint8 { return 0 }

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
func (c *CPU) asl() uint8 { return 0 }

// Branch if Carry Clear
func (c *CPU) bcc() uint8 { return 0 }

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
func (c *CPU) bit() uint8 { return 0 }

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
func (c *CPU) cli() uint8 { return 0 }

// Clear Overflow Flag
func (c *CPU) clv() uint8 { return 0 }

// Compare
func (c *CPU) cmp() uint8 { return 0 }

// Compare X Register
func (c *CPU) cpx() uint8 { return 0 }

// Compare Y Register
func (c *CPU) cpy() uint8 { return 0 }

// Decrement Memory
func (c *CPU) dec() uint8 { return 0 }

// Decrement X Register
func (c *CPU) dex() uint8 { return 0 }

// Decrement Y Register
func (c *CPU) dey() uint8 { return 0 }

// Exclusive OR
func (c *CPU) eor() uint8 { return 0 }

// Increment Memory
func (c *CPU) inc() uint8 { return 0 }

// Increment X Register
func (c *CPU) inx() uint8 { return 0 }

// Increment Y Register
func (c *CPU) iny() uint8 { return 0 }

// Jump
func (c *CPU) jmp() uint8 { return 0 }

// Jump to Subroutine
func (c *CPU) jsr() uint8 { return 0 }

// Load Accumulator
func (c *CPU) lda() uint8 { return 0 }

// Load X Register
func (c *CPU) ldx() uint8 { return 0 }

// Load Y Register
func (c *CPU) ldy() uint8 { return 0 }

// Logical Shift Right
func (c *CPU) lsr() uint8 { return 0 }

// No Operation
func (c *CPU) nop() uint8 { return 0 }

// Logical Inclusive OR
func (c *CPU) ora() uint8 { return 0 }

// Push Accumulator
func (c *CPU) pha() uint8 { return 0 }

// Push Processor Status
func (c *CPU) php() uint8 { return 0 }

// Pull Accumulator
func (c *CPU) pla() uint8 { return 0 }

// Pull Processor Status
func (c *CPU) plp() uint8 { return 0 }

// Rotate Left
func (c *CPU) rol() uint8 { return 0 }

// Rotate Right
func (c *CPU) ror() uint8 { return 0 }

// Return from Interrupt
func (c *CPU) rti() uint8 { return 0 }

// Return from Subroutine
func (c *CPU) rts() uint8 { return 0 }

// Subtract with Carry
func (c *CPU) sbc() uint8 { return 0 }

// Set Carry Flag
func (c *CPU) sec() uint8 { return 0 }

// Set Decimal Flag
func (c *CPU) sed() uint8 { return 0 }

// Set Interrupt Disable
func (c *CPU) sei() uint8 { return 0 }

// Store Accumulator
func (c *CPU) sta() uint8 { return 0 }

// Store X Register
func (c *CPU) stx() uint8 { return 0 }

// Store Y Register
func (c *CPU) sty() uint8 { return 0 }

// Transfer Accumulator to X
func (c *CPU) tax() uint8 { return 0 }

// Transfer Accumulator to Y
func (c *CPU) tay() uint8 { return 0 }

// Transfer Stack Pointer to X
func (c *CPU) tsx() uint8 { return 0 }

// Transfer X to Accumulator
func (c *CPU) txa() uint8 { return 0 }

// Transfer X to Stack Pointer
func (c *CPU) txs() uint8 { return 0 }

// Transfer Y to Accumulator
func (c *CPU) tya() uint8 { return 0 }

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
