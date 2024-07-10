package cpu

import "testing"

func TestSetFlag(t *testing.T) {
	c := &CPU{}

	// Test setting a flag to true
	c.setFlag(flagCBit, true)
	if !c.getFlag(flagCBit) {
		t.Errorf("Expected flagCBit to be true, got false")
	}

	// Test setting a flag to false
	c.setFlag(flagCBit, false)
	if c.getFlag(flagCBit) {
		t.Errorf("Expected flagCBit to be false, got true")
	}

	// Test setting multiple flags
	c.setFlag(flagCBit, true)
	c.setFlag(flagZBit, true)
	c.setFlag(flagNBit, true)
	if !c.getFlag(flagCBit) || !c.getFlag(flagZBit) || !c.getFlag(flagNBit) {
		t.Errorf("Expected flagCBit, flagZBit, and flagNBit to be true, got false")
	}
}
func TestGetFlag(t *testing.T) {
	c := &CPU{
		status: flagCBit | flagZBit | flagNBit,
	}

	// Test getting a true flag
	if !c.getFlag(flagCBit) {
		t.Errorf("Expected flagCBit to be true, got false")
	}

	// Test getting a false flag
	if c.getFlag(flagIBit) {
		t.Errorf("Expected flagIBit to be false, got true")
	}

	// Test getting multiple flags
	if !c.getFlag(flagCBit) || !c.getFlag(flagZBit) || !c.getFlag(flagNBit) {
		t.Errorf("Expected flagCBit, flagZBit, and flagNBit to be true, got false")
	}
}
