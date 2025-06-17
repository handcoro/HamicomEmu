namespace HamicomEmu.Ppu

module Registers =

  module ControlFlags =
    let nameTable1               = 0b0000_0001uy
    let nameTable2               = 0b0000_0010uy
    let vramAddIncrement         = 0b0000_0100uy
    let spritePatternAddress     = 0b0000_1000uy
    let backgroundPatternAddress = 0b0001_0000uy
    let spriteSize               = 0b0010_0000uy
    let masterSlaveSelect        = 0b0100_0000uy
    let generateNmi              = 0b1000_0000uy

  module StatusFlags =
    let ppuOpenBus     = 0b0001_1111uy
    let spriteOverflow = 0b0010_0000uy
    let spriteZeroHit  = 0b0100_0000uy
    let vblank         = 0b1000_0000uy

  module MaskFlags =
    let grayscale                = 0b0000_0001uy
    let showBackgroundInLeftmost = 0b0000_0010uy
    let showSpritesInLeftmost    = 0b0000_0100uy
    let backgroundRendering      = 0b0000_1000uy
    let spriteRendering          = 0b0001_0000uy
    let emphasizeRed             = 0b0010_0000uy
    let emphasizeGreen           = 0b0100_0000uy
    let emphasizeBlue            = 0b1000_0000uy


  type AddressRegister = {
    value: byte * byte
  }

  let initialAddressRegister = {
    value = (0uy, 0uy)
  }

  /// この実装は不正確らしい
  /// v, t, x レジスタとか？
  /// どうやら内部レジスタとしてアドレスレジスタと共有しているらしい
  /// 複雑な分割スクロールを表現するためにはアドレスレジスタとともに内部レジスタに実装を変更するかも
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
