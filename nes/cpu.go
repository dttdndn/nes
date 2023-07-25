package nes

import (
	"encoding/gob"
)

const CPUFrequency = 1789773

// interrupt types
const (
	_ = iota
	interruptNone
	interruptNMI
	interruptIRQ
)

// addressing modes
const (
	_ = iota
	modeAbsolute
	modeAbsoluteX
	modeAbsoluteY
	modeAccumulator
	modeImmediate
	modeImplied
	modeIndexedIndirect
	modeIndirect
	modeIndirectIndexed
	modeRelative
	modeZeroPage
	modeZeroPageX
	modeZeroPageY
)

// instructionModes indicates the addressing mode for each instruction
var instructionModes = [256]byte{
	6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
	1, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
	6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
	6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 8, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
	5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,
	5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,
	5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
	5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
	10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
}

// instructionSizes indicates the size of each instruction in bytes
var instructionSizes = [256]byte{
	2, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
	3, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
	1, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
	1, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 0, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 0, 2, 2, 2, 2, 1, 3, 1, 0, 0, 3, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 0, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 2, 1, 2, 3, 3, 3, 3,
	2, 2, 0, 2, 2, 2, 2, 2, 1, 3, 1, 3, 3, 3, 3, 3,
}

// instructionCycles indicates the number of cycles used by each instruction,
// not including conditional cycles
var instructionCycles = [256]byte{
	7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
}

// instructionPageCycles indicates the number of cycles used by each
// instruction when a page is crossed
var instructionPageCycles = [256]byte{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
}

// instruction IO
const (
	_ = iota
	ioRead
	ioWrite
)

// instructionIO indicates the IO direction(R/W) of an instruction if any
var instructionIO = [256]byte{
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 2, 0, 1, 2, 2, 2, 1, 0, 0, 0, 0, 2, 2, 2, 1,
	0, 2, 0, 0, 2, 2, 2, 1, 0, 2, 0, 0, 0, 2, 0, 0,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
}

// instructionNames indicates the name of each instruction
var instructionNames = [256]string{
	"BRK", "ORA", "KIL", "*SLO", "*NOP", "ORA", "ASL", "*SLO", "PHP", "ORA", "ASL", "ANC", "*NOP", "ORA", "ASL", "*SLO",
	"BPL", "ORA", "KIL", "*SLO", "*NOP", "ORA", "ASL", "*SLO", "CLC", "ORA", "*NOP", "*SLO", "*NOP", "ORA", "ASL", "*SLO",
	"JSR", "AND", "KIL", "*RLA", "BIT", "AND", "ROL", "*RLA", "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "*RLA",
	"BMI", "AND", "KIL", "*RLA", "*NOP", "AND", "ROL", "*RLA", "SEC", "AND", "*NOP", "*RLA", "*NOP", "AND", "ROL", "*RLA",
	"RTI", "EOR", "KIL", "*SRE", "*NOP", "EOR", "LSR", "*SRE", "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "*SRE",
	"BVC", "EOR", "KIL", "*SRE", "*NOP", "EOR", "LSR", "*SRE", "CLI", "EOR", "*NOP", "*SRE", "*NOP", "EOR", "LSR", "*SRE",
	"RTS", "ADC", "KIL", "*RRA", "*NOP", "ADC", "ROR", "*RRA", "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "*RRA",
	"BVS", "ADC", "KIL", "*RRA", "*NOP", "ADC", "ROR", "*RRA", "SEI", "ADC", "*NOP", "*RRA", "*NOP", "ADC", "ROR", "*RRA",
	"*NOP", "STA", "NOP", "*SAX", "STY", "STA", "STX", "*SAX", "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "*SAX",
	"BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "*SAX", "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",
	"LDY", "LDA", "LDX", "*LAX", "LDY", "LDA", "LDX", "*LAX", "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "*LAX",
	"BCS", "LDA", "KIL", "*LAX", "LDY", "LDA", "LDX", "*LAX", "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "*LAX",
	"CPY", "CMP", "NOP", "*DCP", "CPY", "CMP", "DEC", "*DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "*DCP",
	"BNE", "CMP", "KIL", "*DCP", "*NOP", "CMP", "DEC", "*DCP", "CLD", "CMP", "*NOP", "*DCP", "*NOP", "CMP", "DEC", "*DCP",
	"CPX", "SBC", "NOP", "*ISB", "CPX", "SBC", "INC", "*ISB", "INX", "SBC", "NOP", "*SBC", "CPX", "SBC", "INC", "*ISB",
	"BEQ", "SBC", "KIL", "*ISB", "*NOP", "SBC", "INC", "*ISB", "SED", "SBC", "*NOP", "*ISB", "*NOP", "SBC", "INC", "*ISB",
}

type CPU struct {
	Memory           // memory interface
	Cycles    uint64 // number of cycles
	PC        uint16 // program counter
	SP        byte   // stack pointer
	A         byte   // accumulator
	X         byte   // x register
	Y         byte   // y register
	C         byte   // carry flag
	Z         byte   // zero flag
	I         byte   // interrupt disable flag
	D         byte   // decimal mode flag
	B         byte   // break command flag
	U         byte   // unused flag
	V         byte   // overflow flag
	N         byte   // negative flag
	interrupt byte   // interrupt type to perform
	stall     int    // number of cycles to stall
	table     [256]func(*stepInfo)
}

func NewCPU(console *Console) *CPU {
	cpu := CPU{Memory: NewCPUMemory(console)}
	cpu.createTable()
	cpu.Reset()
	return &cpu
}

// createTable builds a function table for each instruction
func (c *CPU) createTable() {
	c.table = [256]func(*stepInfo){
		c.brk, c.ora, c.kil, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.php, c.ora, c.asl, c.anc, c.nop, c.ora, c.asl, c.slo,
		c.bpl, c.ora, c.kil, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.clc, c.ora, c.nop, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.jsr, c.and, c.kil, c.rla, c.bit, c.and, c.rol, c.rla,
		c.plp, c.and, c.rol, c.anc, c.bit, c.and, c.rol, c.rla,
		c.bmi, c.and, c.kil, c.rla, c.nop, c.and, c.rol, c.rla,
		c.sec, c.and, c.nop, c.rla, c.nop, c.and, c.rol, c.rla,
		c.rti, c.eor, c.kil, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.pha, c.eor, c.lsr, c.alr, c.jmp, c.eor, c.lsr, c.sre,
		c.bvc, c.eor, c.kil, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.cli, c.eor, c.nop, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.rts, c.adc, c.kil, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.pla, c.adc, c.ror, c.arr, c.jmp, c.adc, c.ror, c.rra,
		c.bvs, c.adc, c.kil, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.sei, c.adc, c.nop, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.nop, c.sta, c.nop, c.sax, c.sty, c.sta, c.stx, c.sax,
		c.dey, c.nop, c.txa, c.xaa, c.sty, c.sta, c.stx, c.sax,
		c.bcc, c.sta, c.kil, c.ahx, c.sty, c.sta, c.stx, c.sax,
		c.tya, c.sta, c.txs, c.tas, c.shy, c.sta, c.shx, c.ahx,
		c.ldy, c.lda, c.ldx, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.tay, c.lda, c.tax, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.bcs, c.lda, c.kil, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.clv, c.lda, c.tsx, c.las, c.ldy, c.lda, c.ldx, c.lax,
		c.cpy, c.cmp, c.nop, c.dcp, c.cpy, c.cmp, c.dec, c.dcp,
		c.iny, c.cmp, c.dex, c.axs, c.cpy, c.cmp, c.dec, c.dcp,
		c.bne, c.cmp, c.kil, c.dcp, c.nop, c.cmp, c.dec, c.dcp,
		c.cld, c.cmp, c.nop, c.dcp, c.nop, c.cmp, c.dec, c.dcp,
		c.cpx, c.sbc, c.nop, c.isb, c.cpx, c.sbc, c.inc, c.isb,
		c.inx, c.sbc, c.nop, c.sbc, c.cpx, c.sbc, c.inc, c.isb,
		c.beq, c.sbc, c.kil, c.isb, c.nop, c.sbc, c.inc, c.isb,
		c.sed, c.sbc, c.nop, c.isb, c.nop, c.sbc, c.inc, c.isb,
	}
}

func (c *CPU) Save(encoder *gob.Encoder) error {
	encoder.Encode(c.Cycles)
	encoder.Encode(c.PC)
	encoder.Encode(c.SP)
	encoder.Encode(c.A)
	encoder.Encode(c.X)
	encoder.Encode(c.Y)
	encoder.Encode(c.C)
	encoder.Encode(c.Z)
	encoder.Encode(c.I)
	encoder.Encode(c.D)
	encoder.Encode(c.B)
	encoder.Encode(c.U)
	encoder.Encode(c.V)
	encoder.Encode(c.N)
	encoder.Encode(c.interrupt)
	encoder.Encode(c.stall)
	return nil
}

func (c *CPU) Load(decoder *gob.Decoder) error {
	decoder.Decode(&c.Cycles)
	decoder.Decode(&c.PC)
	decoder.Decode(&c.SP)
	decoder.Decode(&c.A)
	decoder.Decode(&c.X)
	decoder.Decode(&c.Y)
	decoder.Decode(&c.C)
	decoder.Decode(&c.Z)
	decoder.Decode(&c.I)
	decoder.Decode(&c.D)
	decoder.Decode(&c.B)
	decoder.Decode(&c.U)
	decoder.Decode(&c.V)
	decoder.Decode(&c.N)
	decoder.Decode(&c.interrupt)
	decoder.Decode(&c.stall)
	return nil
}

// Reset resets the CPU to its initial powerup state
func (c *CPU) Reset() {
	c.PC = c.Read16(0xFFFC)
	c.SP = 0xFD
	c.SetFlags(0x24)
}

// pagesDiffer returns true if the two addresses reference different pages
func pagesDiffer(a, b uint16) bool {
	return a&0xFF00 != b&0xFF00
}

// addBranchCycles adds a cycle for taking a branch and adds another cycle
// if the branch jumps to a new page
func (c *CPU) addBranchCycles(info *stepInfo) {
	c.Cycles++
	if pagesDiffer(info.pc, info.address) {
		c.Cycles++
	}
}

func (c *CPU) compare(a, b byte) {
	c.setZN(a - b)
	if a >= b {
		c.C = 1
	} else {
		c.C = 0
	}
}

// Read16 reads two bytes using Read to return a double-word value
func (c *CPU) Read16(address uint16) uint16 {
	lo := uint16(c.Read(address))
	hi := uint16(c.Read(address + 1))
	return hi<<8 | lo
}

// read16bug emulates a 6502 bug that caused the low byte to wrap without
// incrementing the high byte
func (c *CPU) read16bug(address uint16) uint16 {
	a := address
	b := (a & 0xFF00) | uint16(byte(a)+1)
	lo := c.Read(a)
	hi := c.Read(b)
	return uint16(hi)<<8 | uint16(lo)
}

// push pushes a byte onto the stack
func (c *CPU) push(value byte) {
	c.Write(0x100|uint16(c.SP), value)
	c.SP--
}

// pull pops a byte from the stack
func (c *CPU) pull() byte {
	c.SP++
	return c.Read(0x100 | uint16(c.SP))
}

// push16 pushes two bytes onto the stack
func (c *CPU) push16(value uint16) {
	hi := byte(value >> 8)
	lo := byte(value & 0xFF)
	c.push(hi)
	c.push(lo)
}

// pull16 pops two bytes from the stack
func (c *CPU) pull16() uint16 {
	lo := uint16(c.pull())
	hi := uint16(c.pull())
	return hi<<8 | lo
}

// Flags returns the processor status flags
func (c *CPU) Flags() byte {
	var flags byte
	flags |= c.C << 0
	flags |= c.Z << 1
	flags |= c.I << 2
	flags |= c.D << 3
	flags |= c.B << 4
	flags |= c.U << 5
	flags |= c.V << 6
	flags |= c.N << 7
	return flags
}

// SetFlags sets the processor status flags
func (c *CPU) SetFlags(flags byte) {
	c.C = (flags >> 0) & 1
	c.Z = (flags >> 1) & 1
	c.I = (flags >> 2) & 1
	c.D = (flags >> 3) & 1
	c.B = (flags >> 4) & 1
	c.U = (flags >> 5) & 1
	c.V = (flags >> 6) & 1
	c.N = (flags >> 7) & 1
}

// setZ sets the zero flag if the argument is zero
func (c *CPU) setZ(value byte) {
	if value == 0 {
		c.Z = 1
	} else {
		c.Z = 0
	}
}

// setN sets the negative flag if the argument is negative (high bit is set)
func (c *CPU) setN(value byte) {
	if value&0x80 != 0 {
		c.N = 1
	} else {
		c.N = 0
	}
}

// setZN sets the zero flag and the negative flag
func (c *CPU) setZN(value byte) {
	c.setZ(value)
	c.setN(value)
}

// triggerNMI causes a non-maskable interrupt to occur on the next cycle
func (c *CPU) triggerNMI() {
	c.interrupt = interruptNMI
}

// triggerIRQ causes an IRQ interrupt to occur on the next cycle
func (c *CPU) triggerIRQ() {
	if c.I == 0 {
		c.interrupt = interruptIRQ
	}
}

// stepInfo contains information that the instruction functions use
type stepInfo struct {
	address uint16
	pc      uint16
	mode    byte
}

// Step executes a single CPU instruction
func (c *CPU) Step() int {
	if c.stall > 0 {
		c.stall--
		return 1
	}

	cycles := c.Cycles

	switch c.interrupt {
	case interruptNMI:
		c.nmi()
	case interruptIRQ:
		c.irq()
	}
	c.interrupt = interruptNone

	opcode := c.Read(c.PC)
	mode := instructionModes[opcode]

	address := c.computeStepInfoAddress(c.PC, mode)
	pageCrossed := c.computeStepInfoPageCrossed(address, mode)

	c.PC += uint16(instructionSizes[opcode])
	c.Cycles += uint64(instructionCycles[opcode])
	if pageCrossed {
		c.Cycles += uint64(instructionPageCycles[opcode])
	}
	info := &stepInfo{
		address: address,
		pc:      c.PC,
		mode:    mode,
	}
	c.table[opcode](info)

	return int(c.Cycles - cycles)
}

func (c *CPU) computeStepInfoAddress(pc uint16, mode byte) uint16 {
	switch mode {
	case modeAbsolute:
		return c.Read16(pc + 1)
	case modeAbsoluteX:
		return c.Read16(pc+1) + uint16(c.X)
	case modeAbsoluteY:
		return c.Read16(pc+1) + uint16(c.Y)
	case modeAccumulator:
		return 0
	case modeImmediate:
		return pc + 1
	case modeImplied:
		return 0
	case modeIndexedIndirect:
		return c.read16bug(uint16(c.Read(pc+1) + c.X))
	case modeIndirect:
		return c.read16bug(c.Read16(pc + 1))
	case modeIndirectIndexed:
		return c.read16bug(uint16(c.Read(pc+1))) + uint16(c.Y)
	case modeRelative:
		offset := uint16(c.Read(pc + 1))
		if offset < 0x80 {
			return pc + 2 + offset
		} else {
			return pc + 2 + offset - 0x100
		}
	case modeZeroPage:
		return uint16(c.Read(pc + 1))
	case modeZeroPageX:
		return uint16(c.Read(pc+1)+c.X) & 0xff
	case modeZeroPageY:
		return uint16(c.Read(pc+1)+c.Y) & 0xff
	default:
		return 0
	}
}

func (c *CPU) computeStepInfoPageCrossed(address uint16, mode byte) bool {
	switch mode {
	case modeAbsoluteX:
		return pagesDiffer(address-uint16(c.X), address)
	case modeAbsoluteY:
		return pagesDiffer(address-uint16(c.Y), address)
	case modeIndirectIndexed:
		return pagesDiffer(address-uint16(c.Y), address)
	default:
		return false
	}
}

// NMI - Non-Maskable Interrupt
func (c *CPU) nmi() {
	c.push16(c.PC)
	c.php(nil)
	c.PC = c.Read16(0xFFFA)
	c.I = 1
	c.Cycles += 7
}

// IRQ - IRQ Interrupt
func (c *CPU) irq() {
	c.push16(c.PC)
	c.php(nil)
	c.PC = c.Read16(0xFFFE)
	c.I = 1
	c.Cycles += 7
}

// ADC - Add with Carry
func (c *CPU) adc(info *stepInfo) {
	a := c.A
	b := c.Read(info.address)
	carry := c.C
	c.A = a + b + carry
	c.setZN(c.A)
	if int(a)+int(b)+int(carry) > 0xFF {
		c.C = 1
	} else {
		c.C = 0
	}
	if (a^b)&0x80 == 0 && (a^c.A)&0x80 != 0 {
		c.V = 1
	} else {
		c.V = 0
	}
}

// AND - Logical AND
func (c *CPU) and(info *stepInfo) {
	c.A = c.A & c.Read(info.address)
	c.setZN(c.A)
}

// ASL - Arithmetic Shift Left
func (c *CPU) asl(info *stepInfo) {
	if info.mode == modeAccumulator {
		c.C = (c.A >> 7) & 1
		c.A <<= 1
		c.setZN(c.A)
	} else {
		value := c.Read(info.address)
		c.C = (value >> 7) & 1
		value <<= 1
		c.Write(info.address, value)
		c.setZN(value)
	}
}

// BCC - Branch if Carry Clear
func (c *CPU) bcc(info *stepInfo) {
	if c.C == 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BCS - Branch if Carry Set
func (c *CPU) bcs(info *stepInfo) {
	if c.C != 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BEQ - Branch if Equal
func (c *CPU) beq(info *stepInfo) {
	if c.Z != 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BIT - Bit Test
func (c *CPU) bit(info *stepInfo) {
	value := c.Read(info.address)
	c.V = (value >> 6) & 1
	c.setZ(value & c.A)
	c.setN(value)
}

// BMI - Branch if Minus
func (c *CPU) bmi(info *stepInfo) {
	if c.N != 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BNE - Branch if Not Equal
func (c *CPU) bne(info *stepInfo) {
	if c.Z == 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BPL - Branch if Positive
func (c *CPU) bpl(info *stepInfo) {
	if c.N == 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BRK - Force Interrupt
func (c *CPU) brk(info *stepInfo) {
	c.push16(c.PC)
	c.php(info)
	c.sei(info)
	c.PC = c.Read16(0xFFFE)
}

// BVC - Branch if Overflow Clear
func (c *CPU) bvc(info *stepInfo) {
	if c.V == 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// BVS - Branch if Overflow Set
func (c *CPU) bvs(info *stepInfo) {
	if c.V != 0 {
		c.PC = info.address
		c.addBranchCycles(info)
	}
}

// CLC - Clear Carry Flag
func (c *CPU) clc(*stepInfo) {
	c.C = 0
}

// CLD - Clear Decimal Mode
func (c *CPU) cld(*stepInfo) {
	c.D = 0
}

// CLI - Clear Interrupt Disable
func (c *CPU) cli(*stepInfo) {
	c.I = 0
}

// CLV - Clear Overflow Flag
func (c *CPU) clv(*stepInfo) {
	c.V = 0
}

// CMP - Compare
func (c *CPU) cmp(info *stepInfo) {
	value := c.Read(info.address)
	c.compare(c.A, value)
}

// CPX - Compare X Register
func (c *CPU) cpx(info *stepInfo) {
	value := c.Read(info.address)
	c.compare(c.X, value)
}

// CPY - Compare Y Register
func (c *CPU) cpy(info *stepInfo) {
	value := c.Read(info.address)
	c.compare(c.Y, value)
}

// DEC - Decrement Memory
func (c *CPU) dec(info *stepInfo) {
	value := c.Read(info.address) - 1
	c.Write(info.address, value)
	c.setZN(value)
}

// DEX - Decrement X Register
func (c *CPU) dex(*stepInfo) {
	c.X--
	c.setZN(c.X)
}

// DEY - Decrement Y Register
func (c *CPU) dey(*stepInfo) {
	c.Y--
	c.setZN(c.Y)
}

// EOR - Exclusive OR
func (c *CPU) eor(info *stepInfo) {
	c.A = c.A ^ c.Read(info.address)
	c.setZN(c.A)
}

// INC - Increment Memory
func (c *CPU) inc(info *stepInfo) {
	value := c.Read(info.address) + 1
	c.Write(info.address, value)
	c.setZN(value)
}

// INX - Increment X Register
func (c *CPU) inx(*stepInfo) {
	c.X++
	c.setZN(c.X)
}

// INY - Increment Y Register
func (c *CPU) iny(*stepInfo) {
	c.Y++
	c.setZN(c.Y)
}

// JMP - Jump
func (c *CPU) jmp(info *stepInfo) {
	c.PC = info.address
}

// JSR - Jump to Subroutine
func (c *CPU) jsr(info *stepInfo) {
	c.push16(c.PC - 1)
	c.PC = info.address
}

// LDA - Load Accumulator
func (c *CPU) lda(info *stepInfo) {
	c.A = c.Read(info.address)
	c.setZN(c.A)
}

// LDX - Load X Register
func (c *CPU) ldx(info *stepInfo) {
	c.X = c.Read(info.address)
	c.setZN(c.X)
}

// LDY - Load Y Register
func (c *CPU) ldy(info *stepInfo) {
	c.Y = c.Read(info.address)
	c.setZN(c.Y)
}

// LSR - Logical Shift Right
func (c *CPU) lsr(info *stepInfo) {
	if info.mode == modeAccumulator {
		c.C = c.A & 1
		c.A >>= 1
		c.setZN(c.A)
	} else {
		value := c.Read(info.address)
		c.C = value & 1
		value >>= 1
		c.Write(info.address, value)
		c.setZN(value)
	}
}

// NOP - No Operation
func (c *CPU) nop(*stepInfo) {
}

// ORA - Logical Inclusive OR
func (c *CPU) ora(info *stepInfo) {
	c.A = c.A | c.Read(info.address)
	c.setZN(c.A)
}

// PHA - Push Accumulator
func (c *CPU) pha(*stepInfo) {
	c.push(c.A)
}

// PHP - Push Processor Status
func (c *CPU) php(*stepInfo) {
	c.push(c.Flags() | 0x10)
}

// PLA - Pull Accumulator
func (c *CPU) pla(*stepInfo) {
	c.A = c.pull()
	c.setZN(c.A)
}

// PLP - Pull Processor Status
func (c *CPU) plp(*stepInfo) {
	c.SetFlags(c.pull()&0xEF | 0x20)
}

// ROL - Rotate Left
func (c *CPU) rol(info *stepInfo) {
	if info.mode == modeAccumulator {
		carry := c.C
		c.C = (c.A >> 7) & 1
		c.A = (c.A << 1) | carry
		c.setZN(c.A)
	} else {
		carry := c.C
		value := c.Read(info.address)
		c.C = (value >> 7) & 1
		value = (value << 1) | carry
		c.Write(info.address, value)
		c.setZN(value)
	}
}

// ROR - Rotate Right
func (c *CPU) ror(info *stepInfo) {
	if info.mode == modeAccumulator {
		carry := c.C
		c.C = c.A & 1
		c.A = (c.A >> 1) | (carry << 7)
		c.setZN(c.A)
	} else {
		carry := c.C
		value := c.Read(info.address)
		c.C = value & 1
		value = (value >> 1) | (carry << 7)
		c.Write(info.address, value)
		c.setZN(value)
	}
}

// RTI - Return from Interrupt
func (c *CPU) rti(*stepInfo) {
	c.SetFlags(c.pull()&0xEF | 0x20)
	c.PC = c.pull16()
}

// RTS - Return from Subroutine
func (c *CPU) rts(*stepInfo) {
	c.PC = c.pull16() + 1
}

// SBC - Subtract with Carry
func (c *CPU) sbc(info *stepInfo) {
	a := c.A
	b := c.Read(info.address)
	carry := c.C
	c.A = a - b - (1 - carry)
	c.setZN(c.A)
	if int(a)-int(b)-int(1-carry) >= 0 {
		c.C = 1
	} else {
		c.C = 0
	}
	if (a^b)&0x80 != 0 && (a^c.A)&0x80 != 0 {
		c.V = 1
	} else {
		c.V = 0
	}
}

// SEC - Set Carry Flag
func (c *CPU) sec(*stepInfo) {
	c.C = 1
}

// SED - Set Decimal Flag
func (c *CPU) sed(*stepInfo) {
	c.D = 1
}

// SEI - Set Interrupt Disable
func (c *CPU) sei(*stepInfo) {
	c.I = 1
}

// STA - Store Accumulator
func (c *CPU) sta(info *stepInfo) {
	c.Write(info.address, c.A)
}

// STX - Store X Register
func (c *CPU) stx(info *stepInfo) {
	c.Write(info.address, c.X)
}

// STY - Store Y Register
func (c *CPU) sty(info *stepInfo) {
	c.Write(info.address, c.Y)
}

// TAX - Transfer Accumulator to X
func (c *CPU) tax(*stepInfo) {
	c.X = c.A
	c.setZN(c.X)
}

// TAY - Transfer Accumulator to Y
func (c *CPU) tay(*stepInfo) {
	c.Y = c.A
	c.setZN(c.Y)
}

// TSX - Transfer Stack Pointer to X
func (c *CPU) tsx(*stepInfo) {
	c.X = c.SP
	c.setZN(c.X)
}

// TXA - Transfer X to Accumulator
func (c *CPU) txa(*stepInfo) {
	c.A = c.X
	c.setZN(c.A)
}

// TXS - Transfer X to Stack Pointer
func (c *CPU) txs(*stepInfo) {
	c.SP = c.X
}

// TYA - Transfer Y to Accumulator
func (c *CPU) tya(*stepInfo) {
	c.A = c.Y
	c.setZN(c.A)
}

// illegal opcodes below

func (c *CPU) ahx(*stepInfo) {
}

func (c *CPU) alr(*stepInfo) {
}

func (c *CPU) anc(*stepInfo) {
}

func (c *CPU) arr(*stepInfo) {
}

func (c *CPU) axs(*stepInfo) {
}

func (c *CPU) dcp(info *stepInfo) {
	c.dec(info)
	c.cmp(info)
}

func (c *CPU) isb(info *stepInfo) {
	c.inc(info)
	c.sbc(info)
}

func (c *CPU) kil(*stepInfo) {
}

func (c *CPU) las(*stepInfo) {
}

func (c *CPU) lax(info *stepInfo) {
	c.lda(info)
	c.tax(info)
}

func (c *CPU) rla(info *stepInfo) {
	c.rol(info)
	c.and(info)
}

func (c *CPU) rra(info *stepInfo) {
	c.ror(info)
	c.adc(info)
}

func (c *CPU) sax(info *stepInfo) {
	c.Write(info.address, c.A&c.X)
}

func (c *CPU) shx(*stepInfo) {
}

func (c *CPU) shy(*stepInfo) {
}

func (c *CPU) slo(info *stepInfo) {
	c.asl(info)
	c.ora(info)
}

func (c *CPU) sre(info *stepInfo) {
	c.lsr(info)
	c.eor(info)
}

func (c *CPU) tas(*stepInfo) {
}

func (c *CPU) xaa(*stepInfo) {
}
