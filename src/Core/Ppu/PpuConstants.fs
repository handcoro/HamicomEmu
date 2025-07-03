namespace HamicomEmu.Ppu
module ControlFlags =
  let nameTable1               = 0b0000_0001uy
  let nameTable2               = 0b0000_0010uy
  let vramAddIncrement         = 0b0000_0100uy
  let spritePatternAddress     = 0b0000_1000uy
  let backgroundPatternAddress = 0b0001_0000uy
  let spriteSize               = 0b0010_0000uy
  let masterSlaveSelect        = 0b0100_0000uy
  let generateNmi              = 0b1000_0000uy

module ControlMasks =
  let nameTable                = 0b0000_0011uy

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

module ScrollMasks =
  let nameTable = 0b000_1100_0000_0000us

module SpriteAttributes =
  let paletteIndex attr   = attr &&& 0b11uy
  let priority attr       = attr >>> 5 &&& 1uy
  let flipHorizontal attr = attr >>> 6 &&& 1uy = 1uy
  let flipVertical attr   = attr >>> 7 &&& 1uy = 1uy
