namespace HamicomEmu.Ppu

module Types =
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

    type ShiftRegisters = {
        mutable patternLow: uint16
        mutable patternHigh: uint16
        mutable attrLow: uint16
        mutable attrHigh: uint16
    }

    type PpuLatches = {
        mutable tile: byte
        mutable attr: byte
        mutable patternLow: byte
        mutable patternHigh: byte
    }

    [<Struct>]
    type SpriteInfo = {
        mutable index: int
        mutable y: byte
        mutable tile: byte
        mutable attr: byte
        mutable x: byte
        mutable tileLo: byte
        mutable tileHi: byte
    }

    type PpuState = {
        cartridge: Cartridge
        pal: byte array
        vram: byte array
        oam: byte array
        mutable secondarySprites: SpriteInfo array
        mutable secondarySpritesCount: int
        mutable hasSprite: bool array
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
        mutable workBuffer: byte array
        mutable frameBuffer: byte array
        regs: ShiftRegisters
        latches: PpuLatches
        mutable frameIsOdd: bool
    }

