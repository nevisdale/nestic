package cpu

type opcodeFunc func()

// Load Accumulator - Copies data from memory into the accumulator register.
func (c *CPU) lda() {}

// Load X Register - Copies data from memory into the X register.
func (c *CPU) ldx() {}

// Load Y Register - Copies data from memory into the Y register.
func (c *CPU) ldy() {}

// Store Accumulator - Copies the contents of the accumulator into memory.
func (c *CPU) sta() {}

// Store X Register - Copies the contents of the X register into memory.
func (c *CPU) stx() {}

// Store Y Register - Copies the contents of the Y register into memory.
func (c *CPU) sty() {}

// Add with Carry - Adds the contents of a memory location to the accumulator (with carry).
func (c *CPU) adc() {}

// Subtract with Carry - Subtracts the contents of a memory location from the accumulator (with borrow).
func (c *CPU) sbc() {}

// Compare Accumulator - Compares the accumulator with a memory location.
func (c *CPU) cmp() {}

// Compare X Register - Compares the X register with a memory location.
func (c *CPU) cpx() {}

// Compare Y Register - Compares the Y register with a memory location.
func (c *CPU) cpy() {}

// Logical AND - Performs a bitwise AND operation between the accumulator and a memory location.
func (c *CPU) and() {}

// Logical OR - Performs a bitwise OR operation between the accumulator and a memory location.
func (c *CPU) ora() {}

// Exclusive OR - Performs a bitwise exclusive OR operation between the accumulator and a memory location.
func (c *CPU) eor() {}

// Arithmetic Shift Left - Shifts the bits of the accumulator or a memory location to the left.
func (c *CPU) asl() {}

// Logical Shift Right - Shifts the bits of the accumulator or a memory location to the right.
func (c *CPU) lsr() {}

// Rotate Left - Rotates the bits of the accumulator or a memory location to the left through the carry flag.
func (c *CPU) rol() {}

// Rotate Right - Rotates the bits of the accumulator or a memory location to the right through the carry flag.
func (c *CPU) ror() {}

// Bit Test - Performs a bitwise AND between the accumulator and a memory location, setting flags but not storing the result.
func (c *CPU) bit() {}

// Jump - Transfers program execution to a different part of the program.
func (c *CPU) jmp() {}

// Jump to Subroutine - Jumps to a subroutine and saves the return address.
func (c *CPU) jsr() {}

// Return from Subroutine - Returns from a subroutine to the calling routine.
func (c *CPU) rts() {}

// Break - Initiates an interrupt request.
func (c *CPU) brk() {}

// Return from Interrupt - Returns from an interrupt routine.
func (c *CPU) rti() {}

// Branch if Carry Clear - Performs a conditional branch based on the carry flag.
func (c *CPU) bcc() {}

// Branch if Carry Set - Performs a conditional branch based on the carry flag.
func (c *CPU) bcs() {}

// Branch if Equal - Performs a conditional branch based on the zero flag.
func (c *CPU) beq() {}

// Branch if Minus - Performs a conditional branch based on the negative flag.
func (c *CPU) bmi() {}

// Branch if Not Equal - Performs a conditional branch based on the zero flag.
func (c *CPU) bne() {}

// Branch if Positive - Performs a conditional branch based on the negative flag.
func (c *CPU) bpl() {}

// Branch if Overflow Clear - Performs a conditional branch based on the overflow flag.
func (c *CPU) bvc() {}

// Branch if Overflow Set - Performs a conditional branch based on the overflow flag.
func (c *CPU) bvs() {}

// Clear Carry Flag - Resets the carry flag to 0.
func (c *CPU) clc() {}

// Set Carry Flag - Sets the carry flag to 1.
func (c *CPU) sec() {}

// Clear Decimal Mode - Resets the decimal mode flag to 0.
func (c *CPU) cld() {}

// Set Decimal Mode - Sets the decimal mode flag to 1.
func (c *CPU) sed() {}

// Clear Interrupt Disable - Resets the interrupt disable flag to 0.
func (c *CPU) cli() {}

// Set Interrupt Disable - Sets the interrupt disable flag to 1.
func (c *CPU) sei() {}

// Clear Overflow Flag - Resets the overflow flag to 0.
func (c *CPU) clv() {}

// No Operation - No operation is performed.
func (c *CPU) nop() {}

// Transfer X to Accumulator - Copies the contents of the X register into the accumulator.
func (c *CPU) txa() {}

// Transfer Y to Accumulator - Copies the contents of the Y register into the accumulator.
func (c *CPU) tya() {}

// Transfer X to Stack Pointer - Copies the contents of the X register into the stack pointer.
func (c *CPU) txs() {}

// Transfer Accumulator to Y - Copies the contents of the accumulator into the Y register.
func (c *CPU) tay() {}

// Transfer Accumulator to X - Copies the contents of the accumulator into the X register.
func (c *CPU) tax() {}

// Transfer Stack Pointer to X - Copies the contents of the stack pointer into the X register.
func (c *CPU) tsx() {}

// Push Accumulator - Pushes the contents of the accumulator onto the stack.
func (c *CPU) pha() {}

// Push Processor Status (Flags) - Pushes the processor status (flags) onto the stack.
func (c *CPU) php() {}

// Pull Accumulator - Pulls the contents of the stack into the accumulator.
func (c *CPU) pla() {}

// Pull Processor Status (Flags) - Pulls the processor status (flags) from the stack.
func (c *CPU) plp() {}

// Unknown or unsupported opcode
func (c *CPU) xxx() {}

func (c *CPU) opcodeFuncFromMnemonic(mnemonic string) (opcodeFunc, error) {
	_ = mnemonic
	return nil, nil
}
