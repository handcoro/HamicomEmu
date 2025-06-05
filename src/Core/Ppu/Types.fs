namespace HamicomEmu.Ppu.Types

open HamicomEmu.Ppu.Registers
open HamicomEmu.Cartridge

type PpuState = {
  chr: byte array
  chrRam: byte array
  pal: byte array
  vram: byte array
  oam: byte array
  oamAddr: byte
  mirror: Mirroring
  addrReg: AddressRegister
  scrlReg: ScrollRegister
  ctrl: byte
  mask: byte
  status: byte
  buffer: byte
  scanline: uint16
  cycle: uint
  nmiInterrupt: option<byte>
  clearNmiInterrupt: bool
  latch: bool // PPUSCROLL と PPUADDR のラッチは共有らしい
  mutable scrollPerScanline: ScrollRegister array
}

// type RenderCache = {
//   mutable scrollPerScanline : ScrollRegister array
// }
