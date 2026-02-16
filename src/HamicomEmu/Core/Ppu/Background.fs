namespace HamicomEmu.Ppu

module Background =

    open HamicomEmu.Mapper
    open HamicomEmu.Ppu.Common
    open HamicomEmu.Ppu.Types
    open HamicomEmu.Common.BitUtils

    /// n ピクセル分シフトして空ける
    let inline shiftRegisters n (regs: ShiftRegisters) =
        regs.patternLow <- regs.patternLow <<< n
        regs.patternHigh <- regs.patternHigh <<< n
        regs.attrLow <- regs.attrLow <<< n
        regs.attrHigh <- regs.attrHigh <<< n

    /// ネームテーブル情報のフェッチ
    let fetchTileLatch ppu =
        let v = ppu.scroll.v
        let addr =
            Scroll.getTileIndexAddress v
            |> int
            |> mirrorVramAddr ppu.cartridge.screenMirroring ppu.cartridge.mapper
        Mapper.onPpuFetch addr ppu.cartridge.mapper
        ppu.latches.tile <- Mapper.ppuReadNameTable addr ppu.vram ppu.cartridge

    /// 属性テーブルのフェッチ
    let fetchAttrLatch ppu =
        let v = ppu.scroll.v
        let addr =
            Scroll.getAttributeAddress v
            |> int
            |> mirrorVramAddr ppu.cartridge.screenMirroring ppu.cartridge.mapper

        let attrByte = Mapper.ppuReadNameTable addr ppu.vram ppu.cartridge
        let v = int v
        // 該当シフト量 0: 左上タイル 2: 右上タイル 4: 左下タイル 6: 右下タイル
        let shift = ((v >>> 4) &&& 0b100) ||| (v &&& 0b10)

        Mapper.onPpuFetch addr ppu.cartridge.mapper
        ppu.latches.attr <- (attrByte >>> shift) &&& 0b11uy

    let inline backgroundPatternAddr ctrl =
        if hasFlag ControlFlags.backgroundPatternAddress ctrl then
            0x1000us
        else
            0x0000us

    let inline getPatternAddress ppu =
        let patternBase = backgroundPatternAddr ppu.ctrl |> int
        let tile = ppu.latches.tile |> int
        let fineY = (ppu.scroll.v &&& ScrollMasks.fineY) >>> 12 |> int
        let addr = patternBase + tile * 16 + fineY
        addr

    /// パターンテーブル下位
    let fetchPatternLowLatch ppu =
        let addr = getPatternAddress ppu
        Mapper.onPpuFetch addr ppu.cartridge.mapper
        ppu.latches.patternLow <- Mapper.ppuRead addr ppu.vram ppu.cartridge

    /// パターンテーブル上位
    let fetchPatternHighLatch ppu =
        let addr = getPatternAddress ppu + 8 // 上位バイト
        Mapper.onPpuFetch addr ppu.cartridge.mapper
        ppu.latches.patternHigh <- Mapper.ppuRead addr ppu.vram ppu.cartridge

    /// パターンテーブルをレジスタにロード
    let loadPatternRegsFromLatches ppu =
        let regs = ppu.regs
        let latches = ppu.latches
        // シフトして空いた部分にコピー
        regs.patternLow <- regs.patternLow  ||| uint16 latches.patternLow
        regs.patternHigh <- regs.patternHigh ||| uint16 latches.patternHigh

    /// 属性テーブルをレジスタにロード
    let loadAttrRegsFromLatches ppu =
        let regs = ppu.regs
        // 属性ラッチを 8bit 拡張し reg にセット（属性はラッチの 2 ビット分を 8 ピクセルに展開）
        let attr = ppu.latches.attr
        let attrLow  = if attr &&& 0b01uy <> 0uy then 0xFFus else 0x00us
        let attrHigh = if attr &&& 0b10uy <> 0uy then 0xFFus else 0x00us
        regs.attrLow <- regs.attrLow  ||| attrLow
        regs.attrHigh <- regs.attrHigh ||| attrHigh

    let inline getPaletteIndexFromRegs ppu =
        let regs = ppu.regs
        let offset = int ppu.scroll.x
        let p0 = (regs.patternLow  <<< offset) &&& 0x8000us >>> 15
        let p1 = (regs.patternHigh <<< offset) &&& 0x8000us >>> 15
        let a0 = (regs.attrLow  <<< offset) &&& 0x8000us >>> 15
        let a1 = (regs.attrHigh <<< offset) &&& 0x8000us >>> 15
        (a1 <<< 3) ||| (a0 <<< 2) ||| (p1 <<< 1) ||| p0 |> int

    /// 8 サイクルで 8 ピクセル分のデータを取得する
    let inline loadTiles cycle ppu =
        // 8 サイクルごとに fetch & load
        match cycle &&& 7u with
        | 1u ->
            fetchTileLatch ppu // ネームテーブル
            loadPatternRegsFromLatches ppu
            loadAttrRegsFromLatches ppu // 各ラッチ -> シフトレジスタ
        | 3u -> fetchAttrLatch ppu // 属性テーブル
        | 5u -> fetchPatternLowLatch ppu // パターンテーブル下位
        | 7u -> fetchPatternHighLatch ppu // パターンテーブル上位
        | _ -> ()