namespace HamicomEmu.Ppu.Types

open HamicomEmu.Cartridge
open HamicomEmu.Mapper.Types

/// https://www.nesdev.org/wiki/PPU_scrolling
/// v:
/// yyy NN YYYYY XXXXX
/// ||| || ||||| +++++-- coarse X scroll (5 bits)
/// ||| || +++++-------- coarse Y scroll (5 bits)
/// ||| ++-------------- nametable select (2 bits)
/// +++----------------- fine Y scroll (3 bits)
/// t: 一時データ
/// x: fine X
/// w: 書き込みラッチ
type ScrollRegisters = {
  mutable v: uint16
  t: uint16
  x: byte
  w: bool
}

type PpuState = {
  cartridge: Cartridge
  pal: byte array
  vram: byte array
  oam: byte array
  mutable oamAddr: byte
  scroll: ScrollRegisters
  ctrl: byte
  mask: byte
  mutable status: byte
  buffer: byte
  mutable scanline: uint16
  mutable cycle: uint
  mutable nmiInterrupt: option<byte>
  clearNmiInterrupt: bool
  mutable scrollPerScanline: ScrollRegisters array
  mutable ctrlPerScanline: byte array
  mutable mapperPerScanline: Mapper array
  mutable frameIsOdd: bool
  mutable frameJustCompleted: bool
}

type PpuPublicState = {
  scrollPerScanline: ScrollRegisters array
  ctrlPerScanline: byte array
  mapperPerScanline: Mapper array
}