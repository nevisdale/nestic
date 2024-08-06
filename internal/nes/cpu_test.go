package nes

import (
	v2 "math/rand/v2"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type memMock struct {
	mock.Mock
}

func (m *memMock) Read8(addr uint16) uint8 {
	args := m.Called(addr)
	return args.Get(0).(uint8)
}

func (m *memMock) Write8(addr uint16, data uint8) {
	m.Called(addr, data)
}

func Test_ADC(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initA          uint8
		operandValue   uint8
		initP          uint8
		expectedA      uint8
		expectedP      uint8
		pageCrossed    bool
		expectedCycles uint8
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.a = in.initA
		cpu.p = in.initP
		cpu.operandValue = in.operandValue
		cpu.pageCrossed = in.pageCrossed

		cpu.adc()

		assert.Equal(t, in.expectedA, cpu.a, "A register")
		assert.Equal(t, in.expectedP, cpu.p, "P register")
		assert.Equal(t, in.expectedCycles, cpu.cycles, "Cycles")
	}

	t.Run("zero result, no carry", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0,
			operandValue: 0,
			initP:        0,
			expectedA:    0,
			expectedP:    flagZBit,
		})
	})

	t.Run("simple addition, no carry", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x10,
			operandValue: 0x20,
			initP:        0,
			expectedA:    0x30,
			expectedP:    0,
		})
	})

	t.Run("overflow with carry set", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0x1,
			initP:        0,
			expectedA:    0,
			expectedP:    flagZBit | flagCBit,
		})
	})

	t.Run("negative result with overflow", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x7f,
			operandValue: 0x1,
			initP:        0,
			expectedA:    0x80,
			expectedP:    flagNBit | flagVBit,
		})
	})

	t.Run("simple addition with overflow, result is negative", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x50,
			operandValue: 0x50,
			initP:        0,
			expectedA:    0xa0,
			expectedP:    flagNBit | flagVBit,
		})
	})

	t.Run("addition with carry in, result is negative", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x50,
			operandValue: 0x50,
			initP:        flagCBit,
			expectedA:    0xa1,
			expectedP:    flagNBit | flagVBit,
		})
	})

	t.Run("overflow with carry in, result is positive", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0x1,
			initP:        flagCBit,
			expectedA:    0x01,
			expectedP:    flagCBit,
		})
	})

	t.Run("addition with carry in, zero result", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0x00,
			initP:        flagCBit,
			expectedA:    0x00,
			expectedP:    flagZBit | flagCBit,
		})
	})

	t.Run("add cycle if page crossed", func(t *testing.T) {
		testDo(t, testArgs{
			initA:          0,
			operandValue:   0,
			initP:          0,
			expectedA:      0,
			expectedP:      flagZBit,
			pageCrossed:    true,
			expectedCycles: 1,
		})
	})
}

func Test_AND(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initA          uint8
		operandValue   uint8
		initP          uint8
		expectedA      uint8
		expectedP      uint8
		pageCrossed    bool
		expectedCycles uint8
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.a = in.initA
		cpu.p = in.initP
		cpu.operandValue = in.operandValue
		cpu.pageCrossed = in.pageCrossed

		cpu.and()

		assert.Equal(t, in.expectedA, cpu.a, "A register")
		assert.Equal(t, in.expectedP, cpu.p, "P register")
		assert.Equal(t, in.expectedCycles, cpu.cycles, "Cycles")
	}

	t.Run("ff&0f=0f", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0x0f,
			initP:        0,
			expectedA:    0x0f,
			expectedP:    0,
		})
	})

	t.Run("ff&00=00", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0x00,
			initP:        0,
			expectedA:    0x00,
			expectedP:    flagZBit,
		})
	})

	t.Run("ff&ff=ff", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0xff,
			operandValue: 0xff,
			initP:        0,
			expectedA:    0xff,
			expectedP:    flagNBit,
		})
	})

	t.Run("add cycle if page crossed", func(t *testing.T) {
		testDo(t, testArgs{
			initA:          0,
			operandValue:   0,
			initP:          0,
			expectedA:      0,
			expectedP:      flagZBit,
			pageCrossed:    true,
			expectedCycles: 1,
		})
	})
}

func Test_ASL(t *testing.T) {
	t.Parallel()

	t.Run("ACC with carry", func(t *testing.T) {
		expectedA := uint8(0x6)
		expectedP := flagCBit
		cpu := NewCPU(nil)
		cpu.operandValue = 0x83
		cpu.p = 0
		cpu.addrMode = addrModeACC

		cpu.asl()

		assert.Equal(t, expectedA, cpu.a, "A register")
		assert.Equal(t, expectedP, cpu.p, "P register")
	})

	t.Run("ACC with negative", func(t *testing.T) {
		expectedA := uint8(0x82)
		expectedP := flagNBit
		cpu := NewCPU(nil)
		cpu.operandValue = 0x41
		cpu.p = 0
		cpu.addrMode = addrModeACC

		cpu.asl()

		assert.Equal(t, expectedA, cpu.a, "A register")
		assert.Equal(t, expectedP, cpu.p, "P register")
	})

	t.Run("ACC with zero", func(t *testing.T) {
		expectedA := uint8(0)
		expectedP := flagZBit
		cpu := NewCPU(nil)
		cpu.operandValue = 0x0
		cpu.p = 0
		cpu.addrMode = addrModeACC

		cpu.asl()

		assert.Equal(t, expectedA, cpu.a, "A register")
		assert.Equal(t, expectedP, cpu.p, "P register")
	})

	t.Run("ZP simple", func(t *testing.T) {
		expectedAddr := uint16(0xff)
		expectedValue := uint8(0x24)
		mem := new(memMock)
		mem.On("Write8", expectedAddr, expectedValue).Return()

		expectedP := uint8(0)
		cpu := NewCPU(mem)
		cpu.p = 0
		cpu.operandValue = 0x12
		cpu.operandAddr = expectedAddr
		cpu.addrMode = addrModeZP

		cpu.asl()

		assert.Equal(t, expectedP, cpu.p, "P register")
		mem.AssertExpectations(t)
	})
}

func Test_JmpWithCondition(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initP           uint8
		initPC          uint16
		expectedPC      uint16
		initOperandAddr uint16
		expectedCycles  uint8
	}

	testDo := func(t *testing.T, in testArgs, do func(cpu *CPU)) {
		cpu := NewCPU(nil)
		cpu.p = in.initP
		cpu.pc = in.initPC
		cpu.operandAddr = in.initOperandAddr

		do(cpu)

		assert.Equal(t, in.expectedPC, cpu.pc, "PC register")
		assert.Equal(t, in.expectedCycles, cpu.cycles, "Cycles")
		assert.Equal(t, in.initP, cpu.p, "P register") // P register should not be changed
	}

	noJumpTestArgs := testArgs{
		initP:           0,
		initPC:          0x1000,
		expectedPC:      0x1000,
		initOperandAddr: 0x2000,
		expectedCycles:  0,
	}
	jumpToSamePageTestArgs := testArgs{
		initP:           0,
		initPC:          0x1000,
		expectedPC:      0x1010,
		initOperandAddr: 0x0010,
		expectedCycles:  1,
	}
	jumpToDifferentPageTestArgs := testArgs{
		initP:           0,
		initPC:          0x1000,
		expectedPC:      0x2000,
		initOperandAddr: 0x1000,
		expectedCycles:  2,
	}

	t.Run("BCC", func(t *testing.T) {
		bcc := func(cpu *CPU) { cpu.bcc() }

		arg := noJumpTestArgs
		arg.initP = flagCBit
		testDo(t, arg, bcc)

		arg = jumpToSamePageTestArgs
		arg.initP = ^flagCBit
		testDo(t, arg, bcc)

		arg = jumpToDifferentPageTestArgs
		arg.initP = ^flagCBit
		testDo(t, arg, bcc)
	})

	t.Run("BCS", func(t *testing.T) {
		bcs := func(cpu *CPU) { cpu.bcs() }

		arg := noJumpTestArgs
		arg.initP = ^flagCBit
		testDo(t, arg, bcs)

		arg = jumpToSamePageTestArgs
		arg.initP = flagCBit
		testDo(t, arg, bcs)

		arg = jumpToDifferentPageTestArgs
		arg.initP = flagCBit
		testDo(t, arg, bcs)
	})

	t.Run("BEQ", func(t *testing.T) {
		beq := func(cpu *CPU) { cpu.beq() }

		arg := noJumpTestArgs
		arg.initP = ^flagZBit
		testDo(t, arg, beq)

		arg = jumpToSamePageTestArgs
		arg.initP = flagZBit
		testDo(t, arg, beq)

		arg = jumpToDifferentPageTestArgs
		arg.initP = flagZBit
		testDo(t, arg, beq)
	})

	t.Run("BMI", func(t *testing.T) {
		bmi := func(cpu *CPU) { cpu.bmi() }

		arg := noJumpTestArgs
		arg.initP = ^flagNBit
		testDo(t, arg, bmi)

		arg = jumpToSamePageTestArgs
		arg.initP = flagNBit
		testDo(t, arg, bmi)

		arg = jumpToDifferentPageTestArgs
		arg.initP = flagNBit
		testDo(t, arg, bmi)
	})

	t.Run("BNE", func(t *testing.T) {
		bne := func(cpu *CPU) { cpu.bne() }

		arg := noJumpTestArgs
		arg.initP = flagZBit
		testDo(t, arg, bne)

		arg = jumpToSamePageTestArgs
		arg.initP = ^flagZBit
		testDo(t, arg, bne)

		arg = jumpToDifferentPageTestArgs
		arg.initP = ^flagZBit
		testDo(t, arg, bne)
	})

	t.Run("BPL", func(t *testing.T) {
		bpl := func(cpu *CPU) { cpu.bpl() }

		arg := noJumpTestArgs
		arg.initP = flagNBit
		testDo(t, arg, bpl)

		arg = jumpToSamePageTestArgs
		arg.initP = ^flagNBit
		testDo(t, arg, bpl)

		arg = jumpToDifferentPageTestArgs
		arg.initP = ^flagNBit
		testDo(t, arg, bpl)
	})

	t.Run("BVC", func(t *testing.T) {
		bvc := func(cpu *CPU) { cpu.bvc() }

		arg := noJumpTestArgs
		arg.initP = flagVBit
		testDo(t, arg, bvc)

		arg = jumpToSamePageTestArgs
		arg.initP = ^flagVBit
		testDo(t, arg, bvc)

		arg = jumpToDifferentPageTestArgs
		arg.initP = ^flagVBit
		testDo(t, arg, bvc)
	})

	t.Run("BVS", func(t *testing.T) {
		bvs := func(cpu *CPU) { cpu.bvs() }

		arg := noJumpTestArgs
		arg.initP = ^flagVBit
		testDo(t, arg, bvs)

		arg = jumpToSamePageTestArgs
		arg.initP = flagVBit
		testDo(t, arg, bvs)

		arg = jumpToDifferentPageTestArgs
		arg.initP = flagVBit
		testDo(t, arg, bvs)
	})
}

func Test_Bit(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initA        uint8
		operandValue uint8
		initP        uint8
		expectedP    uint8
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.a = in.initA
		cpu.p = in.initP
		cpu.operandValue = in.operandValue

		cpu.bit()

		assert.Equal(t, in.expectedP, cpu.p, "P register")
	}

	t.Run("set Z bit", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x0,
			operandValue: 0x12,
			initP:        0,
			expectedP:    flagZBit,
		})
	})

	t.Run("set N bit", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x80,
			operandValue: 0x81,
			initP:        0,
			expectedP:    flagNBit,
		})
	})

	t.Run("set V bit", func(t *testing.T) {
		testDo(t, testArgs{
			initA:        0x40,
			operandValue: 0x48,
			initP:        0,
			expectedP:    flagVBit,
		})
	})
}

func Test_SetClearP(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initP     uint8
		expectedP uint8
	}

	testDo := func(t *testing.T, in testArgs, do func(cpu *CPU)) {
		cpu := NewCPU(nil)
		cpu.p = in.initP

		do(cpu)

		assert.Equal(t, in.expectedP, cpu.p, "P register")
	}

	randomP := func() uint8 {
		return uint8(v2.N(0x100))
	}

	testsClear := []struct {
		name        string
		f           uint8
		cpuDo       func(cpu *CPU)
		expectedPDo func(p uint8) uint8
	}{
		{
			name:        "CLC",
			f:           flagCBit,
			cpuDo:       func(cpu *CPU) { cpu.clc() },
			expectedPDo: func(p uint8) uint8 { return p & ^flagCBit },
		},
		{
			name:        "CLD",
			f:           flagDBit,
			cpuDo:       func(cpu *CPU) { cpu.cld() },
			expectedPDo: func(p uint8) uint8 { return p & ^flagDBit },
		},
		{
			name:        "CLI",
			f:           flagIBit,
			cpuDo:       func(cpu *CPU) { cpu.cli() },
			expectedPDo: func(p uint8) uint8 { return p & ^flagIBit },
		},
		{
			name:        "CLV",
			f:           flagVBit,
			cpuDo:       func(cpu *CPU) { cpu.clv() },
			expectedPDo: func(p uint8) uint8 { return p & ^flagVBit },
		},
		{
			name:        "SEC",
			f:           flagCBit,
			cpuDo:       func(cpu *CPU) { cpu.sec() },
			expectedPDo: func(p uint8) uint8 { return p | flagCBit },
		},
		{
			name:        "SED",
			f:           flagDBit,
			cpuDo:       func(cpu *CPU) { cpu.sed() },
			expectedPDo: func(p uint8) uint8 { return p | flagDBit },
		},
		{
			name:        "SEI",
			f:           flagIBit,
			cpuDo:       func(cpu *CPU) { cpu.sei() },
			expectedPDo: func(p uint8) uint8 { return p | flagIBit },
		},
	}

	for _, tt := range testsClear {
		t.Run(tt.name, func(t *testing.T) {
			p := randomP() | tt.f
			testDo(t, testArgs{initP: p, expectedP: tt.expectedPDo(p)}, tt.cpuDo)

			p = randomP() & ^tt.f
			testDo(t, testArgs{initP: p, expectedP: tt.expectedPDo(p)}, tt.cpuDo)
		})
	}
}

func Test_BRK(t *testing.T) {
	t.Parallel()

	initPc := uint16(0x1000)
	initP := uint8(v2.N(0x100)) & ^flagBBit
	initSp := uint8(0x10)
	expectedPc := uint16(0xdead)
	expectedP := initP | flagIBit
	mem := new(memMock)

	cpu := NewCPU(mem)
	cpu.pc = initPc
	cpu.p = initP
	cpu.sp = initSp

	// push pc to stack
	initPc++
	mem.On("Write8", uint16(initSp)|uint16(stackStartAddr), uint8(initPc>>8)).Return() // high byte
	initSp--
	mem.On("Write8", uint16(initSp)|uint16(stackStartAddr), uint8(initPc)).Return() // low byte
	initSp--

	// push p to stack
	mem.On("Write8", uint16(initSp)|stackStartAddr, initP|flagBBit).Return()

	// read pc from vector
	mem.On("Read8", uint16(0xfffe)).Return(uint8(expectedPc))      // low byte
	mem.On("Read8", uint16(0xffff)).Return(uint8(expectedPc >> 8)) // high byte

	cpu.brk()

	assert.Equal(t, expectedPc, cpu.pc, "PC register")
	assert.Equal(t, expectedP, cpu.p, "P register")

	mem.AssertExpectations(t)
}

func Test_CMP(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initA          uint8
		operandValue   uint8
		initP          uint8
		expectedP      uint8
		expectedCycles uint8
		pageCrossed    bool
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.a = in.initA
		cpu.p = in.initP
		cpu.operandValue = in.operandValue
		cpu.pageCrossed = in.pageCrossed

		cpu.cmp()

		assert.Equal(t, in.expectedP, cpu.p, "P register")
		assert.Equal(t, in.expectedCycles, cpu.cycles, "Cycles")
	}

	t.Run("A=operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit | flagZBit) & ^flagNBit

		testDo(t, testArgs{
			initA:        0x10,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("A>operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit) & ^(flagNBit | flagZBit)

		testDo(t, testArgs{
			initA:        0x10,
			operandValue: 0x0f,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("A<operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagNBit) & ^(flagCBit | flagZBit)

		testDo(t, testArgs{
			initA:        0x0f,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("add cycle if page crossed", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit | flagZBit) & ^flagNBit

		testDo(t, testArgs{
			initA:          0x10,
			operandValue:   0x10,
			initP:          p,
			expectedP:      expectedP,
			expectedCycles: 1,
			pageCrossed:    true,
		})
	})
}

func Test_CPX(t *testing.T) {
	t.Parallel()

	type testArgs struct {
		initX        uint8
		operandValue uint8
		initP        uint8
		expectedP    uint8
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.x = in.initX
		cpu.p = in.initP
		cpu.operandValue = in.operandValue

		cpu.cpx()

		assert.Equal(t, in.expectedP, cpu.p, "P register")
	}

	t.Run("X=operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit | flagZBit) & ^flagNBit

		testDo(t, testArgs{
			initX:        0x10,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("X>operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit) & ^(flagNBit | flagZBit)

		testDo(t, testArgs{
			initX:        0x10,
			operandValue: 0x0f,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("X<operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagNBit) & ^(flagCBit | flagZBit)

		testDo(t, testArgs{
			initX:        0x0f,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})
}

func Test_CPY(t *testing.T) {
	type testArgs struct {
		initY        uint8
		operandValue uint8
		initP        uint8
		expectedP    uint8
	}

	testDo := func(t *testing.T, in testArgs) {
		cpu := NewCPU(nil)
		cpu.y = in.initY
		cpu.p = in.initP
		cpu.operandValue = in.operandValue

		cpu.cpy()

		assert.Equal(t, in.expectedP, cpu.p, "P register")
	}

	t.Run("Y=operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit | flagZBit) & ^flagNBit

		testDo(t, testArgs{
			initY:        0x10,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("Y>operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagCBit) & ^(flagNBit | flagZBit)

		testDo(t, testArgs{
			initY:        0x10,
			operandValue: 0x0f,
			initP:        p,
			expectedP:    expectedP,
		})
	})

	t.Run("Y<operand", func(t *testing.T) {
		p := uint8(v2.N(0x100))
		expectedP := (p | flagNBit) & ^(flagCBit | flagZBit)

		testDo(t, testArgs{
			initY:        0x0f,
			operandValue: 0x10,
			initP:        p,
			expectedP:    expectedP,
		})
	})
}

func Test_DEC(t *testing.T) {
	t.Parallel()

	t.Run("set N", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagNBit) & ^flagZBit
		expectedAddr := uint16(0xff)
		expectedValue := uint8(0x00)
		mem := new(memMock)
		mem.On("Write8", expectedAddr, expectedValue-1).Return()

		cpu := NewCPU(mem)
		cpu.p = initP
		cpu.operandValue = expectedValue
		cpu.operandAddr = expectedAddr

		cpu.dec()

		assert.Equal(t, expectedP, cpu.p, "P register")
		mem.AssertExpectations(t)
	})

	t.Run("set Z", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagZBit) & ^flagNBit
		expectedAddr := uint16(0xff)
		expectedValue := uint8(0x01)
		mem := new(memMock)
		mem.On("Write8", expectedAddr, expectedValue-1).Return()

		cpu := NewCPU(mem)
		cpu.p = initP
		cpu.operandValue = expectedValue
		cpu.operandAddr = expectedAddr

		cpu.dec()

		assert.Equal(t, expectedP, cpu.p, "P register")
		mem.AssertExpectations(t)
	})
}

func Test_DEX(t *testing.T) {
	t.Parallel()

	t.Run("set N", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagNBit) & ^flagZBit
		expectedValue := uint8(0x00)

		cpu := NewCPU(nil)
		cpu.p = initP
		cpu.x = expectedValue

		cpu.dex()

		assert.Equal(t, expectedP, cpu.p, "P register")
		assert.Equal(t, expectedValue-1, cpu.x, "X register")
	})

	t.Run("set Z", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagZBit) & ^flagNBit
		expectedValue := uint8(0x01)

		cpu := NewCPU(nil)
		cpu.p = initP
		cpu.x = expectedValue

		cpu.dex()

		assert.Equal(t, expectedP, cpu.p, "P register")
		assert.Equal(t, expectedValue-1, cpu.x, "X register")
	})
}

func Test_DEY(t *testing.T) {
	t.Parallel()

	t.Run("set N", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagNBit) & ^flagZBit
		expectedValue := uint8(0x00)

		cpu := NewCPU(nil)
		cpu.p = initP
		cpu.y = expectedValue

		cpu.dey()

		assert.Equal(t, expectedP, cpu.p, "P register")
		assert.Equal(t, expectedValue-1, cpu.y, "Y register")
	})

	t.Run("set Z", func(t *testing.T) {
		initP := uint8(v2.N(0x100))
		expectedP := (initP | flagZBit) & ^flagNBit
		expectedValue := uint8(0x01)

		cpu := NewCPU(nil)
		cpu.p = initP
		cpu.y = expectedValue

		cpu.dey()

		assert.Equal(t, expectedP, cpu.p, "P register")
		assert.Equal(t, expectedValue-1, cpu.y, "Y register")
	})
}
