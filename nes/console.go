package nes

import (
	"encoding/gob"
	"fmt"
	"image"
	"image/color"
	"os"
	"path"
)

type Console struct {
	CPU         *CPU
	APU         *APU
	PPU         *PPU
	Cartridge   *Cartridge
	Controller1 *Controller
	Controller2 *Controller
	Mapper      Mapper
	RAM         []byte
}

func NewConsole(path string) (*Console, error) {
	cartridge, err := LoadNESFile(path)
	if err != nil {
		return nil, err
	}
	ram := make([]byte, 2048)
	controller1 := NewController()
	controller2 := NewController()
	console := Console{
		nil, nil, nil, cartridge, controller1, controller2, nil, ram}
	mapper, err := NewMapper(&console)
	if err != nil {
		return nil, err
	}
	console.Mapper = mapper
	console.CPU = NewCPU(&console)
	console.APU = NewAPU(&console)
	console.PPU = NewPPU(&console)
	return &console, nil
}

func (console *Console) Reset() {
	console.CPU.Reset()
}

func (console *Console) Step() int {
	cpuCycles := console.CPU.Step()
	ppuCycles := cpuCycles * 3
	for i := 0; i < ppuCycles; i++ {
		console.PPU.Step()
		console.Mapper.Step()
	}
	for i := 0; i < cpuCycles; i++ {
		console.APU.Step()
	}
	return cpuCycles
}

func (console *Console) StepFrame() int {
	cpuCycles := 0
	frame := console.PPU.Frame
	for frame == console.PPU.Frame {
		cpuCycles += console.Step()
	}
	return cpuCycles
}

func (console *Console) StepSeconds(seconds float64) {
	cycles := int(CPUFrequency * seconds)
	for cycles > 0 {
		cycles -= console.Step()
	}
}

func (console *Console) Buffer() *image.RGBA {
	return console.PPU.front
}

func (console *Console) BackgroundColor() color.RGBA {
	return Palette[console.PPU.readPalette(0)%64]
}

func (console *Console) SetButtons1(buttons [8]bool) {
	console.Controller1.SetButtons(buttons)
}

func (console *Console) SetButtons2(buttons [8]bool) {
	console.Controller2.SetButtons(buttons)
}

func (console *Console) SetAudioChannel(channel chan float32) {
	console.APU.channel = channel
}

func (console *Console) SetAudioSampleRate(sampleRate float64) {
	if sampleRate != 0 {
		// Convert samples per second to cpu steps per sample
		console.APU.sampleRate = CPUFrequency / sampleRate
		// Initialize filters
		console.APU.filterChain = FilterChain{
			HighPassFilter(float32(sampleRate), 90),
			HighPassFilter(float32(sampleRate), 440),
			LowPassFilter(float32(sampleRate), 14000),
		}
	} else {
		console.APU.filterChain = nil
	}
}
func (console *Console) SaveState(filename string) error {
	dir, _ := path.Split(filename)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	encoder := gob.NewEncoder(file)
	return console.Save(encoder)
}

func (console *Console) Save(encoder *gob.Encoder) error {
	encoder.Encode(console.RAM)
	console.CPU.Save(encoder)
	console.APU.Save(encoder)
	console.PPU.Save(encoder)
	console.Cartridge.Save(encoder)
	console.Mapper.Save(encoder)
	return encoder.Encode(true)
}

func (console *Console) LoadState(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	decoder := gob.NewDecoder(file)
	return console.Load(decoder)
}

func (console *Console) Load(decoder *gob.Decoder) error {
	decoder.Decode(&console.RAM)
	console.CPU.Load(decoder)
	console.APU.Load(decoder)
	console.PPU.Load(decoder)
	console.Cartridge.Load(decoder)
	console.Mapper.Load(decoder)
	var dummy bool
	if err := decoder.Decode(&dummy); err != nil {
		return err
	}
	return nil
}

// FormatState return the current console/CPU state as a string
func (console *Console) FormatState() string {
	cpu, ppu := console.CPU, console.PPU
	opcode := cpu.Read(cpu.PC)
	size, name, mode := instructionSizes[opcode], instructionNames[opcode], instructionModes[opcode]
	b0, b1, b2 := cpu.Read(cpu.PC+0), cpu.Read(cpu.PC+1), cpu.Read(cpu.PC+2)
	w0, w1, w2 := fmt.Sprintf("%02X", b0), fmt.Sprintf("%02X", b1), fmt.Sprintf("%02X", b2)
	if size < 2 {
		w1 = "  "
	}
	if size < 3 {
		w2 = "  "
	}
	var operand string
	switch mode {
	case modeAbsolute:
		operand = fmt.Sprintf("$%02X%02X", b2, b1)
	case modeAbsoluteX:
		operand = fmt.Sprintf("$%02X%02X,X", b2, b1)
	case modeAbsoluteY:
		operand = fmt.Sprintf("$%02X%02X,Y", b2, b1)
	case modeAccumulator:
		operand = fmt.Sprintf("A")
	case modeImmediate:
		operand = fmt.Sprintf("#$%02X", b1)
	case modeImplied:
		// nothing
	case modeIndirect:
		operand = fmt.Sprintf("($%02x%02X)", b2, b1)
	case modeIndexedIndirect:
		operand = fmt.Sprintf("($%02X,X)", b1)
	case modeIndirectIndexed:
		operand = fmt.Sprintf("($%02X),Y", b1)
	case modeRelative:
		operand = fmt.Sprintf("$%04X", uint16(int(cpu.PC)+int(int8(b1))+int(size)))
	case modeZeroPage:
		operand = fmt.Sprintf("$%02X", b1)
	case modeZeroPageX:
		operand = fmt.Sprintf("$%02X,X", b1)
	case modeZeroPageY:
		operand = fmt.Sprintf("$%02X,Y", b1)
	}
	misc := ""
	io := instructionIO[opcode]
	if io != 0 {
		address := cpu.computeStepInfoAddress(cpu.PC, mode)
		switch mode {
		case modeZeroPageX:
			misc = fmt.Sprintf(" @ %02X = %02X", address, cpu.Read(address))
		case modeZeroPageY:
			misc = fmt.Sprintf(" @ %02X = %02X", address, cpu.Read(address))
		case modeAbsoluteX:
			misc = fmt.Sprintf(" @ %04X = %02X", address, cpu.Read(address))
		case modeAbsoluteY:
			misc = fmt.Sprintf(" @ %04X = %02X", address, cpu.Read(address))
		case modeIndexedIndirect:
			misc = fmt.Sprintf(" @ %02X = %04X = %02X", b1+cpu.X, address, cpu.Read(address))
		case modeIndirect:
			misc = fmt.Sprintf(" = %04X", address)
		case modeIndirectIndexed:
			indexedAddress := uint16(cpu.Read(uint16(b1+1)))<<8 | uint16(cpu.Read(uint16(b1)))
			misc = fmt.Sprintf(" = %04X @ %04X = %02X", indexedAddress, address, cpu.Read(address))
		default:
			misc = fmt.Sprintf(" = %02X", cpu.Read(address))
		}
	}

	return fmt.Sprintf(
		"%04X  %s %s %s %4s %-28s"+
			"A:%02X X:%02X Y:%02X P:%02X SP:%02X PPU:%3d,%3d CYC:%d",
		cpu.PC, w0, w1, w2, name, operand+misc,
		cpu.A, cpu.X, cpu.Y, cpu.Flags(), cpu.SP, ppu.ScanLine, ppu.Cycle, cpu.Cycles)
}
