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
        mutable t: uint16
        mutable x: byte
        mutable w: bool
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
        mutable cartridge: Cartridge
        pal: byte array
        vram: byte array
        oam: byte array
        mutable secondarySpritesRender: SpriteInfo array
        mutable secondarySpritesRenderCount: int
        mutable secondarySpritesEval: SpriteInfo array
        mutable secondarySpritesEvalCount: int
        mutable hasSpriteRender: bool array
        mutable hasSpriteEval: bool array
        mutable oamAddr: byte
        scroll: ScrollRegisters
        mutable ctrl: byte
        mutable mask: byte
        mutable status: byte
        mutable buffer: byte
        mutable scanline: uint16
        mutable cycle: uint
        mutable nmiInterrupt: option<byte>
        clearNmiInterrupt: bool
        mutable workBuffer: byte array
        mutable frameBuffer: byte array
        regs: ShiftRegisters
        latches: PpuLatches
        mutable frameIsOdd: bool
        // 65-256 逐次スプライト評価用の状態
        mutable evalPrimaryIdx: int        // Primary OAM の現在インデックス（0-63）
        mutable evalSecondaryIdx: int     // Secondary OAM への書き込みカウント（0-7）
        mutable evalReadData: byte        // 現在読込データ
        mutable evalOddCycle: bool        // 奇数=read, 偶数=write
        mutable evalActive: bool          // 評価フェーズ中か（c=65-256）
        mutable evalBytePhase: int        // 0=Y, 1=Tile, 2=Attr, 3=X
        mutable evalLatchY: byte
        mutable evalLatchTile: byte
        mutable evalLatchAttr: byte
        mutable evalLatchX: byte
    }

