package nes

import (
	"encoding/json"
	"os"
	"path"
	"strconv"
	"testing"

	"golang.org/x/exp/maps"
)

func Test_CPU_SingleStepTest(t *testing.T) {
	t.Parallel()

	type cpuState struct {
		PC uint16 `json:"pc"`
		S  uint8  `json:"s"`
		A  uint8  `json:"a"`
		X  uint8  `json:"x"`
		Y  uint8  `json:"y"`
		P  uint8  `json:"p"`

		// slice of elements where
		// element[0] is address
		// element[1] is value
		RAM [][]uint16 `json:"ram"`
	}

	type testInstance struct {
		Name    string   `json:"name"`
		Initial cpuState `json:"initial"`
		Final   cpuState `json:"final"`

		// slice of elements where
		// element[0] is address
		// element[1] is value
		// element[2] is operation (read/write)
		Cycles [][]any `json:"cycles"`
	}

	dir := os.Getenv("SINGLE_STEP_TEST_DIR")
	if dir == "" {
		t.Skip("skipping test because SINGLE_STEP_TEST_DIR is not set")
		return
	}

	files, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}

	mem := newMemMock(t)
	doTest := func(t *testing.T, test testInstance) {
		// init memory
		mem.reset()
		for _, addrVal := range test.Initial.RAM {
			addr := addrVal[0]
			data := uint8(addrVal[1])
			mem.set(addr, data)
		}
		for _, cyc := range test.Cycles {
			op := cyc[2].(string)
			addr := uint16(cyc[0].(float64))
			data := uint8(cyc[1].(float64))
			mem.allow(op, addr, data)
		}

		// init CPU
		cpu := NewCPU(mem)
		cpu.pc = test.Initial.PC
		cpu.sp = test.Initial.S
		cpu.a = test.Initial.A
		cpu.x = test.Initial.X
		cpu.y = test.Initial.Y
		cpu.p = test.Initial.P

		// run CPU
		cpu.Tic()

		// check final state of CPU
		if cpu.pc != test.Final.PC {
			t.Fatalf("expected PC %04X, got %04X", test.Final.PC, cpu.pc)
		}
		if cpu.sp != test.Final.S {
			t.Fatalf("expected S %02X, got %02X", test.Final.S, cpu.sp)
		}
		if cpu.a != test.Final.A {
			t.Fatalf("expected A %02X, got %02X", test.Final.A, cpu.a)
		}
		if cpu.x != test.Final.X {
			t.Fatalf("expected X %02X, got %02X", test.Final.X, cpu.x)
		}
		if cpu.y != test.Final.Y {
			t.Fatalf("expected Y %02X, got %02X", test.Final.Y, cpu.y)
		}
		if cpu.p != test.Final.P {
			t.Fatalf("expected P %02X, got %02X", test.Final.P, cpu.p)
		}

		// check final state of memory
		for _, addrVal := range test.Final.RAM {
			addr := addrVal[0]
			data := uint8(addrVal[1])
			mem.mustBe(addr, data)
		}
	}

	var tests []testInstance
	for _, file := range files {
		tests = tests[:0]

		opcodeStr := path.Base(file.Name())[:2]
		opcode, err := strconv.ParseUint(opcodeStr, 16, 8)
		if err != nil {
			t.Fatalf("failed to parse opcode from file name %s: %v", file.Name(), err)
		}

		fileData, err := os.ReadFile(dir + "/" + file.Name())
		if err != nil {
			t.Fatalf("failed to read file %s: %v", file.Name(), err)
		}

		tests = tests[:0]
		err = json.Unmarshal(fileData, &tests)
		if err != nil {
			t.Fatalf("failed to unmarshal file %s: %v", file.Name(), err)
		}

		t.Run(file.Name(), func(t *testing.T) {
			if !opcodeIsSupported(uint8(opcode)) {
				t.Skipf("skipping test for opcode %02X because it is not supported", opcode)
				return
			}
			for _, test := range tests {
				doTest(t, test)
			}
		})
	}
}

type memMock struct {
	t       *testing.T
	data    []uint8
	allowed map[uint32]struct{}
}

func newMemMock(t *testing.T) *memMock {
	return &memMock{
		t:       t,
		data:    make([]uint8, 0x10000),
		allowed: make(map[uint32]struct{}),
	}
}

func (m *memMock) asUint32(_ string, addr uint16, data uint8) uint32 {
	return uint32(addr) | uint32(data)<<16
}

func (m *memMock) allow(op string, addr uint16, data uint8) {
	m.allowed[m.asUint32(op, addr, data)] = struct{}{}
}

func (m *memMock) mustBe(addr uint16, data uint8) {
	if m.data[addr] != data {
		m.t.Fatalf("expected %02X at address %04X, got %02X", data, addr, m.data[addr])
	}
}

func (m *memMock) set(addr uint16, data uint8) {
	m.data[addr] = data
}

func (m *memMock) reset() {
	for i := range m.data {
		m.data[i] = 0
	}
	maps.Clear(m.allowed)
}

func (m *memMock) Read8(addr uint16) uint8 {
	// do not check because read does not change memory
	return m.data[addr]
}

func (m *memMock) Write8(addr uint16, data uint8) {
	_, ok := m.allowed[m.asUint32("write", addr, data)]
	if !ok {
		m.t.Fatalf("not allowed write to address %04X with value %02X", addr, data)
	}
	m.data[addr] = data
}

func TestCPU_Reset(t *testing.T) {
	expectedPC := uint16(0x8000)
	expectedP := flagU | flagI
	expectedSP := uint8(0xfd)
	expectedCycles := uint8(7)

	mem := newMemMock(t)
	mem.set(0xfffc, uint8(expectedPC>>0)) // lsb initial PC
	mem.set(0xfffd, uint8(expectedPC>>8)) // msb initial PC

	cpu := NewCPU(mem)
	cpu.Reset()

	// check initial state of CPU
	if cpu.pc != expectedPC {
		t.Fatalf("expected PC to be %04X, got %04X", expectedPC, cpu.pc)
	}
	if cpu.sp != expectedSP {
		t.Fatalf("expected SP to be %02X, got %02X", expectedSP, cpu.sp)
	}
	if cpu.p != expectedP {
		t.Fatalf("expected P to be %02X, got %02X", expectedP, cpu.p)
	}
	if cpu.cycles != expectedCycles {
		t.Fatalf("expected cycles to be %d, got %d", expectedCycles, cpu.cycles)
	}
}
