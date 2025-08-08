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
    let nameTable = 0b0000_0011uy

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
    let coarseX    = 0b000_00_00000_11111us
    let coarseY    = 0b000_00_11111_00000us
    let nameTableX = 0b000_01_00000_00000us
    let nameTableY = 0b000_10_00000_00000us
    let nameTable  = 0b000_11_00000_00000us
    let addressing = 0b000_11_11111_11111us
    let fineY      = 0b111_00_00000_00000us

module SpriteAttributes =
    let paletteIndex attr = attr &&& 0b11uy
    let priority attr =
        attr &&& 0b0010_0000uy >>> 5
    let flipHorizontal attr =
        attr &&& 0b0100_0000uy <> 0uy
    let flipVertical attr =
        attr &&& 0b1000_0000uy <> 0uy
