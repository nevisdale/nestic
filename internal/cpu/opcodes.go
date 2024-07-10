package cpu

import (
	"fmt"
	"strings"
)

type opcodeFunc func()

// Add with Carry
func (c *CPU) adc() {}

// Logical AND
func (c *CPU) and() {}

// Arithmetic Shift Left
func (c *CPU) asl() {}

// Branch if Carry Clear
func (c *CPU) bcc() {}

// Branch if Carry Set
func (c *CPU) bcs() {}

// Branch if Equal
func (c *CPU) beq() {}

// Bit Test
func (c *CPU) bit() {}

// Branch if Minus
func (c *CPU) bmi() {}

// Branch if Not Equal
func (c *CPU) bne() {}

// Branch if Positive
func (c *CPU) bpl() {}

// Force Interrupt
func (c *CPU) brk() {}

// Branch if Overflow Clear
func (c *CPU) bvc() {}

// Branch if Overflow Set
func (c *CPU) bvs() {}

// Clear Carry Flag
func (c *CPU) clc() {}

// Clear Decimal Mode
func (c *CPU) cld() {}

// Clear Interrupt Disable
func (c *CPU) cli() {}

// Clear Overflow Flag
func (c *CPU) clv() {}

// Compare
func (c *CPU) cmp() {}

// Compare X Register
func (c *CPU) cpx() {}

// Compare Y Register
func (c *CPU) cpy() {}

// Decrement Memory
func (c *CPU) dec() {}

// Decrement X Register
func (c *CPU) dex() {}

// Decrement Y Register
func (c *CPU) dey() {}

// Exclusive OR
func (c *CPU) eor() {}

// Increment Memory
func (c *CPU) inc() {}

// Increment X Register
func (c *CPU) inx() {}

// Increment Y Register
func (c *CPU) iny() {}

// Jump
func (c *CPU) jmp() {}

// Jump to Subroutine
func (c *CPU) jsr() {}

// Load Accumulator
func (c *CPU) lda() {}

// Load X Register
func (c *CPU) ldx() {}

// Load Y Register
func (c *CPU) ldy() {}

// Logical Shift Right
func (c *CPU) lsr() {}

// No Operation
func (c *CPU) nop() {}

// Logical Inclusive OR
func (c *CPU) ora() {}

// Push Accumulator
func (c *CPU) pha() {}

// Push Processor Status
func (c *CPU) php() {}

// Pull Accumulator
func (c *CPU) pla() {}

// Pull Processor Status
func (c *CPU) plp() {}

// Rotate Left
func (c *CPU) rol() {}

// Rotate Right
func (c *CPU) ror() {}

// Return from Interrupt
func (c *CPU) rti() {}

// Return from Subroutine
func (c *CPU) rts() {}

// Subtract with Carry
func (c *CPU) sbc() {}

// Set Carry Flag
func (c *CPU) sec() {}

// Set Decimal Flag
func (c *CPU) sed() {}

// Set Interrupt Disable
func (c *CPU) sei() {}

// Store Accumulator
func (c *CPU) sta() {}

// Store X Register
func (c *CPU) stx() {}

// Store Y Register
func (c *CPU) sty() {}

// Transfer Accumulator to X
func (c *CPU) tax() {}

// Transfer Accumulator to Y
func (c *CPU) tay() {}

// Transfer Stack Pointer to X
func (c *CPU) tsx() {}

// Transfer X to Accumulator
func (c *CPU) txa() {}

// Transfer X to Stack Pointer
func (c *CPU) txs() {}

// Transfer Y to Accumulator
func (c *CPU) tya() {}

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
