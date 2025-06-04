namespace HamicomEmu.Ppu

module Registers =

  module ControlFlags =
    let Nametable1               = 0b0000_0001uy
    let Nametable2               = 0b0000_0010uy
    let VramAddIncrement         = 0b0000_0100uy
    let SpritePatternAddress     = 0b0000_1000uy
    let BackgroundPatternAddress = 0b0001_0000uy
    let SpriteSize               = 0b0010_0000uy
    let MasterSlaveSelect        = 0b0100_0000uy
    let GenerateNmi              = 0b1000_0000uy

  module StatusFlags =
    let PpuOpenBus     = 0b0001_1111uy
    let SpriteOverflow = 0b0010_0000uy
    let SpriteZeroHit  = 0b0100_0000uy
    let Vblank         = 0b1000_0000uy

  module MaskFlags =
    let Grayscale                = 0b0000_0001uy
    let ShowBackgroundInLeftmost = 0b0000_0010uy
    let ShowSpritesInLeftmost    = 0b0000_0100uy
    let BackgroundRendering      = 0b0000_1000uy
    let SpriteRendering          = 0b0001_0000uy
    let EmphasizeRed             = 0b0010_0000uy
    let EmphasizeGreen           = 0b0100_0000uy
    let EmphasizeBlue            = 0b1000_0000uy


  type AddressRegister = {
    value: byte * byte
  }

  let initialAddressRegister = {
    value = (0uy, 0uy)
  }

  /// この実装は不正確らしい
  /// v, t, x レジスタとか？
  /// https://www.nesdev.org/wiki/PPU_scrolling
  /// yyy NN YYYYY XXXXX
  /// ||| || ||||| +++++-- coarse X scroll (5 bits)
  /// ||| || +++++-------- coarse Y scroll (5 bits)
  /// ||| ++-------------- nametable select (2 bits)
  /// +++----------------- fine Y scroll (3 bits)
  type ScrollRegister = {
    xy: byte * byte
  }

  let initialScrollRegister = {
    xy = (0uy, 0uy)
  }
