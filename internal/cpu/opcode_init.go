package cpu

import (
	"bytes"
	_ "embed"
	"encoding/csv"
	"fmt"
	"io"
	"strconv"
	"strings"
)

//go:embed opcode_matrix.csv
var opcodeMatrixFileData []byte

func (c *CPU) parseOpcodeMatrix() error {
	r := csv.NewReader(bytes.NewReader(opcodeMatrixFileData))
	_, _ = r.Read() // skip header

	if len(c.instructions) != 0x100 {
		c.instructions = make([]instruction, 0x100)
	}

	r.ReuseRecord = true

	for {
		record, err := r.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			return fmt.Errorf("couldn't read data from csv: %w", err)
		}
		if len(record) == 0 {
			continue
		}

		if len(record) != 4 {
			return fmt.Errorf("invalid format for the record: %s: must be 4 parts", strings.Join(record, string(r.Comma)))
		}

		opcodeByte, err := strconv.ParseUint(record[0], 0, 8)
		if err != nil {
			return fmt.Errorf("invalid format for opcode byte: %w", err)
		}

		opcodeFunc, err := c.opcodeFuncFromMnemonic(record[1])
		if err != nil {
			return fmt.Errorf("invalid format for mnemonic: %w", err)
		}

		addressMode, err := addrModeFromString(record[2])
		if err != nil {
			return fmt.Errorf("invalid format for address mode: %w", err)
		}

		cycles, err := strconv.ParseUint(record[3], 0, 8)
		if err != nil {
			return fmt.Errorf("invalid format for opcode cycles: %w", err)
		}

		c.instructions[opcodeByte] = instruction{
			name:     record[1],
			operate:  opcodeFunc,
			addrMode: addressMode,
			cycles:   uint8(cycles),
		}
	}

	return nil
}
