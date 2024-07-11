package cpu

import "fmt"

type addrMode string

const (
	// Immediate: IMM
	//
	// Description: in this mode, the operand is a specific value specified directly in the instruction.
	// For example, LDA #$10 loads the accumulator (A) with the value 10.
	//
	// Format: #$nn, where $nn is the immediate value (byte) specified directly in the instruction.
	addrModeIMM addrMode = "IMM"

	// Zero Page: ZP
	//
	// Description: in this mode, the operand points to an address within the zero page of memory,
	// which saves memory and speeds up execution of instructions.
	// For example, LDA $20 loads the accumulator (A) from the address $20.
	//
	// Format: $nn, where $nn is the address within the first 256 bytes of memory (zero page).
	addrModeZP addrMode = "ZP"

	// Zero Page Indexed with X: ZPX
	//
	// Description: In this mode, the operand is located in the zero page of memory,
	// offset by the value of register X.
	// This speeds up access to data stored in the zero page and simplifies programming.
	// For example, LDA $20,X loads the accumulator (A) from the address $20 + X.
	//
	// Format: $nn,X, where $nn is an address within the first 256 bytes of memory (zero page),
	// and X is added to this address.
	addrModeZPX addrMode = "ZPX"

	// Zero Page Indexed with Y: ZPY
	//
	// Description: In this addressing mode, the operand is located in the zero page of memory,
	// offset by the value of the Y register.
	// This allows for efficient access to data stored in the zero page using the Y register as an index.
	// For example, LDA $20,Y loads the accumulator (A) from the address $20 + Y.
	//
	// Format: $nn,Y, where $nn is an address within the first 256 bytes of memory (zero page),
	// and Y is added to this address.
	addrModeZPY addrMode = "ZPY"

	// Absolute: ABS
	//
	// Description: In this mode, the operand points to a specific address in the NES memory,
	// which can be anywhere within the 64 KB address space.
	// For example, LDA $1234 loads the accumulator (A) from address $1234.
	//
	// Format: $nnnn, where $nnnn is the full 16-bit memory address.
	addrModeABS addrMode = "ABS"

	// Absolute Indexed with X: ABSX
	//
	// Description: In this addressing mode, the operand specifies a specific address in NES memory,
	// and X is added to this address.
	// This allows efficient handling of data and code located in different parts of NES memory.
	// For example, LDA $1234,X loads the accumulator (A) from the address $1234 + X.
	//
	// Format: $nnnn,X, where $nnnn is the full 16-bit memory address, and X is added to this address.
	addrModeABSX addrMode = "ABSX"

	// Absolute Indexed with Y: ABSY
	//
	// Description: In this addressing mode, the operand specifies a specific address in NES memory,
	// and Y is added to this address.
	// This mode is commonly used for accessing data structures and arrays where the base address is fixed,
	// but the index Y allows flexibility in accessing different elements of the structure.
	// For example, LDA $1234,Y loads the accumulator (A) from the address $1234 + Y.
	//
	// Format: $nnnn,Y, where $nnnn is the full 16-bit memory address, and Y is added to this address.
	addrModeABSY addrMode = "ABSY"

	// Indirect: IND
	//
	// Description: In this addressing mode, the operand specifies an address
	// that itself points to another address in memory where the actual operand is stored.
	// This mode is used for implementing jump tables and certain types of data structures.
	// For example, JMP ($1234) jumps to the address stored at memory location $1234.
	//
	// Format: ($nnnn), where $nnnn is the full 16-bit memory address containing
	// the address of the actual operand.
	addrModeIND addrMode = "IND"

	// Indexed Indirect (X): INDX
	//
	// Description: In this addressing mode, the operand is located in the zero page of memory,
	// where $nn serves as a base address, and X is used as an offset to access the actual operand address.
	// This mode is useful for working with tables and arrays of data stored in the zero page,
	// where X serves as an index to access different elements of the structure.
	// For example, LDA ($20,X) loads the accumulator (A) from the address obtained by adding X to $20.
	//
	// Format: ($nn,X), where $nn is a memory address (zero page), and X is added to this address.
	addrModeINDX addrMode = "INDX"

	// Indirect Indexed (Y): INDY
	// Description: In this addressing mode, the operand is located in the zero page of memory,
	// and $nn specifies the base address.
	// The CPU first reads the 16-bit address stored at $nn and then adds
	// the Y register to this address to access the actual operand.
	// This mode is commonly used for accessing elements in arrays or data structures stored in memory.
	// For example, LDA ($20),Y loads the accumulator (A) from the address obtained by
	// adding Y to the address stored at $20.
	//
	// Format: ($nn),Y, where $nn is a memory address (zero page), and Y is added to
	// the address obtained from the zero page.
	addrModeINDY addrMode = "INDY"

	// Relative: REL
	//
	// Description: In this addressing mode, the operand specifies a relative offset
	// from the address of the instruction following the branch instruction.
	// This mode is typically used for conditional branches within the program flow.
	// For example, BEQ $10 will branch to an address that is $10 bytes away from the current instruction
	// if the zero flag is set.
	//
	// Format: $nn, where $nn is a signed 8-bit value (positive or negative).
	addrModeREL addrMode = "REL"

	// Accumulator: ACC
	//
	// Description: In this addressing mode, operations are performed directly on
	// the contents of the accumulator (A). The accumulator is a special
	// register in the CPU used for arithmetic and logical operations,
	// as well as for storing intermediate results.
	// Instructions that operate on the accumulator typically do not require an explicit operand
	// because they directly manipulate its contents.
	// For example: ADC $20 - an instruction that adds the contents of memory address $20 to the accumulator.
	//
	// Format: No explicit operand is required, as instructions operate directly on the contents of the accumulator.
	addrModeACC addrMode = "ACC"

	// Implied: IMP
	//
	// Description: In this addressing mode, the operation implicitly affects
	// certain registers or flags without explicitly specifying an operand.
	// Instructions in this mode typically have fixed behavior defined by the instruction set architecture.
	// For example, CLC (Clear Carry Flag) is an implied mode instruction that clears the carry flag
	// without needing to specify any operands.
	//
	// Format: No operand is explicitly specified in the instruction.
	addrModeIMP addrMode = "IMP"
)

func addrModeFromString(s string) (addrMode, error) {
	switch s {
	case string(addrModeIMM):
		return addrModeIMM, nil
	case string(addrModeZP):
		return addrModeZP, nil
	case string(addrModeZPX):
		return addrModeZPX, nil
	case string(addrModeZPY):
		return addrModeZPY, nil
	case string(addrModeABS):
		return addrModeABS, nil
	case string(addrModeABSX):
		return addrModeABSX, nil
	case string(addrModeABSY):
		return addrModeABSY, nil
	case string(addrModeIND):
		return addrModeIND, nil
	case string(addrModeINDX):
		return addrModeINDX, nil
	case string(addrModeINDY):
		return addrModeINDY, nil
	case string(addrModeREL):
		return addrModeREL, nil
	case string(addrModeACC):
		return addrModeACC, nil
	case string(addrModeIMP):
		return addrModeIMP, nil
	}
	return addrMode("UNKNOWN"), fmt.Errorf("address mode couldn't be parsed from %s", s)
}

// TODO: it is just a prototype for testing purposes
func (c *CPU) doAddressMode(addrMode addrMode) uint8 {
	switch addrMode {
	case addrModeIMM:
		c.addrAbs = c.pc
		c.pc++
		return 0

	case addrModeZP:
		c.addrAbs = uint16(c.bus.Read8(c.pc))
		c.addrAbs &= 0x00FF
		c.pc++
		return 0

	case addrModeZPX:
		c.addrAbs = uint16(c.bus.Read8(c.pc) + c.regX)
		c.addrAbs &= 0x00FF
		c.pc++
		return 0

	case addrModeZPY:
		c.addrAbs = uint16(c.bus.Read8(c.pc) + c.regY)
		c.addrAbs &= 0x00FF
		c.pc++
		return 0

	case addrModeABS:
		c.addrAbs = uint16(c.bus.Read16(c.pc))
		c.pc += 2
		return 0

	case addrModeABSX:
		baseAddr := uint16(c.bus.Read16(c.pc))
		c.pc += 2
		c.addrAbs = baseAddr + uint16(c.regX)

		// if baseAddr points to a different page than c.addrAbs, return 1
		// page number is the high byte of the address
		// for example: 0x1234, 0x12 is the page number
		if (baseAddr & 0xFF00) != (c.addrAbs & 0xFF00) {
			return 1
		}
		return 0

	case addrModeABSY:
		baseAddr := uint16(c.bus.Read16(c.pc))
		c.pc += 2
		c.addrAbs = baseAddr + uint16(c.regY)

		// if baseAddr points to a different page than c.addrAbs, return 1
		// page number is the high byte of the address
		// for example: 0x1234, 0x12 is the page number
		if (baseAddr & 0xFF00) != (c.addrAbs & 0xFF00) {
			return 1
		}
		return 0

	// 0x00ff bug bevare
	// only JMP instruction uses this mode
	case addrModeIND:
		c.addrAbs = c.bus.Read16(c.pc)
		c.pc += 2

		// simulate 6502 page boundary hardware bug
		//
		// if the address is 0x00FF, the low byte is 0xFF and the high byte is 0x00
		addrHi := (c.addrAbs & 0xFF00) | (c.addrAbs&0x00FF + 1)
		c.addrAbs = uint16(c.bus.Read8(c.addrAbs)) | uint16(c.bus.Read8(addrHi))<<8
		return 0

	case addrModeINDX:
		addr := uint16(c.bus.Read8(c.pc)) + uint16(c.regX)
		c.pc++

		// addr is 0x00FF
		// X is 0x0
		// loAddr is 0x00FF
		// hiAddr is 0x0000 (0x00FF + 1 & 0x00FF = 0x0000)
		lo := uint16(c.bus.Read8(addr & 0x00FF))
		hi := uint16(c.bus.Read8((addr + 1) & 0x00FF))
		c.addrAbs = lo | hi<<8
		return 0

	case addrModeINDY:
		addr := uint16(c.bus.Read8(c.pc)) & 0x00FF
		c.pc++

		lo := uint16(c.bus.Read8(addr))
		hi := uint16(c.bus.Read8((addr + 1) & 0x00FF))
		c.addrAbs = lo | hi<<8
		c.addrAbs += uint16(c.regY)

		// return 1 if page boundary is crossed
		// page number is the high byte of the address
		// example: 0x1234, 0x12 is the page number
		if c.addrAbs>>8 != hi {
			return 1
		}
		return 0

	case addrModeREL:
		c.addrRel = uint16(c.bus.Read8(c.pc))
		c.pc++

		// if negative, add leading 1 bits to save the sign
		//
		// 0x80 is 1000_0000 in binary. it means the number (8bit) is negative
		if c.addrRel&0x80 > 0 {
			c.addrRel |= 0xFF00
		}
		return 0

	case addrModeACC:
		// TODO: should we do nothing here?
		c.fetched = c.regA
		return 0

	case addrModeIMP:
		// TODO: should we do nothing here?
		c.fetched = c.regA
		return 0
	}
	panic("unknown address mode")
}
