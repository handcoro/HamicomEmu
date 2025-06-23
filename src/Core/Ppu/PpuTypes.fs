namespace HamicomEmu.Ppu.Types

open HamicomEmu.Ppu.Registers
open HamicomEmu.Cartridge

type PpuState = {
  chr: byte array
  chrRam: byte array
  pal: byte array
  vram: byte array
  oam: byte array
  mutable oamAddr: byte
  mirror: Mirroring
  addrReg: AddressRegister
  scrlReg: ScrollRegister
  ctrl: byte
  mask: byte
  mutable status: byte
  buffer: byte
  mutable scanline: uint16
  mutable cycle: uint
  mutable nmiInterrupt: option<byte>
  clearNmiInterrupt: bool
  latch: bool // PPUSCROLL と PPUADDR のラッチは共有らしい
  mutable scrollPerScanline: ScrollRegister array
  mutable ctrlPerScanline: byte array
  mutable frameIsOdd: bool
}
