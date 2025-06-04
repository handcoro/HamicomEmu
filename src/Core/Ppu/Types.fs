module HamicomEmu.Ppu.Types

open HamicomEmu.Ppu.Registers
open HamicomEmu.Cartridge

type NesPpu = {
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
  cycles: uint
  nmiInterrupt: option<byte>
  clearNmiInterrupt: bool
  latch: bool // PPUSCROLL と PPUADDR のラッチは共有らしい
}

let initialPpu (rom: Rom) = {
  chr = rom.chrRom
  chrRam = rom.chrRam
  pal = Array.create 32 0uy // パレットテーブルは32バイト
  vram = Array.create 0x2000 0uy // PPU VRAM は8KB
  oam = Array.create 256 0uy // OAM データは256バイト
  oamAddr = 0uy
  mirror = rom.screenMirroring
  addrReg = initialAddressRegister
  scrlReg = initialScrollRegister
  ctrl = 0uy // 初期状態では制御レジスタは0
  mask = 0b0001_0000uy
  status = 0uy
  buffer = 0uy
  scanline = 0us
  cycles = 0u
  nmiInterrupt = None
  clearNmiInterrupt = false
  latch = true
}
