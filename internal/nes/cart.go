package nes

import (
	"encoding/binary"
	"fmt"
	"io"
	"os"
)

const (
	inesMagic        = 0x1a53454e
	prgBankSizeBytes = 0x4000
	chrBankSizeBytes = 0x2000
)

type Cart struct {
	pgrMem []uint8
	chrMem []uint8

	pgrBanks uint8
	chrBanks uint8
	mapperID uint8
	mirror   uint8 // 0: horizontal, 1: vertical

	mapper Mapper
}

// NewCartFromFile reads a .nes file and returns a Cart struct.
// Supported NES format: iNES
func NewCartFromFile(path string) (*Cart, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("couldn't open the file: %s", err)
	}
	defer file.Close()

	var header struct {
		Magic      uint32
		PrgRomSize uint8
		ChrRomSize uint8
		Flags6     uint8
		Flags7     uint8
		Flags8     uint8
		Flags9     uint8
		Flags10    uint8
		_          [5]uint8 // unused
	}
	if err := binary.Read(file, binary.LittleEndian, &header); err != nil {
		return nil, fmt.Errorf("couldn't read the header: %s", err)
	}
	if header.Magic != inesMagic {
		return nil, fmt.Errorf("invalid header")
	}
	// the second bit of flags6 is the trainer flag
	if header.Flags6&0x4 != 0 {
		if _, err := file.Seek(512, io.SeekCurrent); err != nil {
			return nil, fmt.Errorf("couldn't skip the trainer: %s", err)
		}
	}

	// flag6 and flag7 contain part of the mapper ID in 4 high bits
	// flag6: lower 4 bits of mapper ID
	// flag7: upper 4 bits of mapper ID
	mapperID := (header.Flags7 & 0xf0) | (header.Flags6 >> 4)

	cart := &Cart{
		pgrMem:   make([]uint8, int(header.PrgRomSize)*prgBankSizeBytes),
		chrMem:   make([]uint8, int(header.ChrRomSize)*chrBankSizeBytes),
		pgrBanks: header.PrgRomSize,
		chrBanks: header.ChrRomSize,
		mapperID: mapperID,
	}
	cart.mapper = NewMapper(cart)
	cart.mirror = header.Flags6 & 0x1

	if n, err := file.Read(cart.pgrMem); n != len(cart.pgrMem) || err != nil {
		if err == nil {
			err = fmt.Errorf("expected %d bytes, read %d bytes", len(cart.pgrMem), n)
		}
		return nil, fmt.Errorf("couldn't read PRG ROM: %s", err)
	}
	if n, err := file.Read(cart.chrMem); n != len(cart.chrMem) || err != nil {
		if err == nil {
			err = fmt.Errorf("expected %d bytes, read %d bytes", len(cart.chrMem), n)
		}
		return nil, fmt.Errorf("couldn't read CHR ROM: %s", err)
	}

	return cart, nil
}

func (c Cart) Read8(addr uint16) uint8 {
	return c.mapper.Read8(addr)
}

func (c Cart) Write8(addr uint16, data uint8) {
	c.mapper.Write8(addr, data)
}
