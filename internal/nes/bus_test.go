package nes

import (
	"os"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_BusTic_Nestest(t *testing.T) {
	nestestBinFile := os.Getenv("NESTEST_BIN")
	nestestLogFile := os.Getenv("NESTEST_LOG")
	if nestestBinFile == "" || nestestLogFile == "" {
		t.Skip("skipping test because NESTEST_BIN or NESTEST_LOG is not set")
		return
	}

	cart, err := NewCartFromFile(nestestBinFile)
	if err != nil {
		t.Fatal("Failed to load nestest rom:", err)
	}

	bus := NewBus()
	bus.LoadCart(cart)
	// nestest (all tests) starts at 0xC000
	bus.cpu.pc = 0xC000

	re := regexp.MustCompile(`([A-F0-9]{4}).+A:([A-F0-9]{2}) X:([A-F0-9]{2}) Y:([A-F0-9]{2}) P:([A-F0-9]{2}) SP:([A-F0-9]{2}).+CYC:(\d+)`)
	type state struct {
		pc uint16
		// before executing the instruction
		a   uint8
		x   uint8
		y   uint8
		sp  uint8
		p   uint8
		cyc uint64
	}

	parseLogLine := func(s string) state {
		match := re.FindStringSubmatch(s)

		// from 1 to skip full match
		pc, err := strconv.ParseUint(match[1], 16, 16)
		if err != nil {
			t.Fatal(err)
		}

		a, err := strconv.ParseUint(match[2], 16, 8)
		if err != nil {
			t.Fatal(err)
		}
		x, err := strconv.ParseUint(match[3], 16, 8)
		if err != nil {
			t.Fatal(err)
		}
		y, err := strconv.ParseUint(match[4], 16, 8)
		if err != nil {
			t.Fatal(err)
		}
		p, err := strconv.ParseUint(match[5], 16, 8)
		if err != nil {
			t.Fatal(err)
		}
		sp, err := strconv.ParseUint(match[6], 16, 8)
		if err != nil {
			t.Fatal(err)
		}
		cyc, err := strconv.ParseUint(match[7], 10, 64)
		if err != nil {
			t.Fatal(err)
		}
		return state{
			pc:  uint16(pc),
			a:   uint8(a),
			x:   uint8(x),
			y:   uint8(y),
			sp:  uint8(sp),
			p:   uint8(p),
			cyc: cyc,
		}
	}

	logFileData, err := os.ReadFile(nestestLogFile)
	if err != nil {
		t.Fatal("Failed to open nestest log file:", err)
	}

	var expectedStates []state
	for _, line := range strings.Split(string(logFileData), "\n") {
		if len(line) == 0 {
			continue
		}
		expectedStates = append(expectedStates, parseLogLine(line))
	}

	for i, expectedState := range expectedStates {
		cyc := bus.cpu.Tic()
		// skip cycles until the next instruction
		for cyc > 0 {
			cyc = bus.cpu.Tic()
		}

		actualState := state{
			pc:  bus.cpu.pc,
			a:   bus.cpu.a,
			x:   bus.cpu.x,
			y:   bus.cpu.y,
			sp:  bus.cpu.sp,
			p:   bus.cpu.p,
			cyc: bus.cpu.totalCycles,
		}
		if !assert.Equal(t, expectedState, actualState, "failed at instruction %s:%d", nestestLogFile, i) {
			return
		}
	}
}
