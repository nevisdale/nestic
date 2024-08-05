package nes

import (
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
