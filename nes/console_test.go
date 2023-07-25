package nes

import (
	"os"
	"strings"
	"testing"
)

func TestNESTest(t *testing.T) {

	bs, err := os.ReadFile("../nestest/nestest.log")
	if err != nil {
		t.Fatal(err)
	}
	logs := strings.Split(string(bs), "\n")

	console, err := NewConsole("../nestest/nestest.nes")
	if err != nil {
		t.Fatal(err)
	}

	// setup the initial state to match the logs
	console.CPU.PC = 0xC000
	console.CPU.Cycles = 7
	console.PPU.ScanLine = 0
	console.PPU.Cycle = 21

	for i, log := range logs {
		got := console.FormatState()
		if got != log {
			t.Logf("at line %d, expected:\n%s", i+1, log)
			t.Logf("but got:\n%s\n", got)
			t.FailNow()
		}
		console.Step()
	}

}
