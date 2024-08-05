package nes

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

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
