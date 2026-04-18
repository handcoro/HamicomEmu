namespace HamicomEmu.Mapper

/// TODO: サブマッパーの実装
module Mmc3 =

    open HamicomEmu.Mapper.Common

    [<Literal>]
    let private ppuCyclesPerFrame = 262 * 341

    [<Literal>]
    let private minA12LowCycles = 8

    type State = {
        mutable mirroring: Mirroring
        hardwiredMirroring: Mirroring

        // バンク選択
        mutable bankSelect: byte
        bankRegs: byte array // 8個
        prgOffsets: int array // 8KB x 4 バンクの先頭を指す
        chrOffsets: int array // 1KB x 8 バンクの先頭を指す

        // IRQ
        mutable irqReloadValue: byte
        mutable irqCounter: byte
        mutable irqReload: bool
        mutable irqEnabled: bool
        mutable irqPending: bool // Bus 側で拾う

        prgRam: byte array

        mutable a12LastHigh: bool      // 前回観測の A12 値
        mutable a12LowCycles: int      // A12 low の累積 PPU cycle
        mutable lastPpuTick: int       // 前回通知の PPU frame 内 tick

        mutable isSaved: bool

    }

    let private clockIrqCounter s =
        if s.irqCounter = 0uy || s.irqReload then
            s.irqCounter <- s.irqReloadValue
            s.irqReload <- false
            if s.irqCounter = 0uy && s.irqEnabled then
                s.irqPending <- true
        else
            s.irqCounter <- s.irqCounter - 1uy
            if s.irqCounter = 0uy && s.irqEnabled then
                s.irqPending <- true

    /// PPU アドレスの A12 立ち上がりを検出し、IRQ カウンタを進める
    let clockIrqFromPpuAddress ppuAddr ppuTick s =
        let a12High = ppuAddr &&& 0x1000 <> 0
        let wasHigh = s.a12LastHigh

        // PPU tick の前回からの差分算出
        let delta =
            if s.lastPpuTick < 0 then
                0
            elif ppuTick >= s.lastPpuTick then
                ppuTick - s.lastPpuTick
            else
                // フレーム跨ぎ
                ppuTick + ppuCyclesPerFrame - s.lastPpuTick

        if not a12High then
            s.a12LowCycles <- min 64 (s.a12LowCycles + delta)

        let lowCyclesSufficient = s.a12LowCycles >= minA12LowCycles
        let risingEdge = (not wasHigh) && a12High && lowCyclesSufficient

        if a12High then
            s.a12LowCycles <- 0

        s.a12LastHigh <- a12High
        s.lastPpuTick <- ppuTick

        if risingEdge then
            clockIrqCounter s


    // 7  bit  0
    // ---- ----
    // CPMx xRRR
    // |||   |||
    // |||   +++- Specify which bank register to update on next write to Bank Data register
    // |||          000: R0: Select 2 KB CHR bank at PPU $0000-$07FF (or $1000-$17FF)
    // |||          001: R1: Select 2 KB CHR bank at PPU $0800-$0FFF (or $1800-$1FFF)
    // |||          010: R2: Select 1 KB CHR bank at PPU $1000-$13FF (or $0000-$03FF)
    // |||          011: R3: Select 1 KB CHR bank at PPU $1400-$17FF (or $0400-$07FF)
    // |||          100: R4: Select 1 KB CHR bank at PPU $1800-$1BFF (or $0800-$0BFF)
    // |||          101: R5: Select 1 KB CHR bank at PPU $1C00-$1FFF (or $0C00-$0FFF)
    // |||          110: R6: Select 8 KB PRG ROM bank at $8000-$9FFF (or $C000-$DFFF)
    // |||          111: R7: Select 8 KB PRG ROM bank at $A000-$BFFF
    // ||+------- Nothing on the MMC3, see MMC6
    // |+-------- PRG ROM bank mode (0: $8000-$9FFF swappable,
    // |                                $C000-$DFFF fixed to second-last bank;
    // |                             1: $C000-$DFFF swappable,
    // |                                $8000-$9FFF fixed to second-last bank)
    // +--------- CHR A12 inversion (0: two 2 KB banks at $0000-$0FFF,
    //                                  four 1 KB banks at $1000-$1FFF;
    //                               1: two 2 KB banks at $1000-$1FFF,
    //                                  four 1 KB banks at $0000-$0FFF)

    let inline private chrOffset (chr : byte[]) bank =
        let size = 1 * 1024
        let totalBanks = chr.Length / size
        getOffset (bank % totalBanks) size 0
    
    let inline private prgOffset (prg: byte[]) bank =
        let size = 8 * 1024
        let totalBanks = prg.Length / size
        getOffset (bank % totalBanks) size 0

    let calcBanks (prg: byte[]) (chr : byte[]) s =
        let chrMode = s.bankSelect &&& 0b1000_0000uy <> 0uy
        let prgMode = s.bankSelect &&& 0b0100_0000uy <> 0uy

        let r0 = int s.bankRegs[0]
        let r1 = int s.bankRegs[1]
        let r2 = int s.bankRegs[2]
        let r3 = int s.bankRegs[3]
        let r4 = int s.bankRegs[4]
        let r5 = int s.bankRegs[5]

        let setChr idx bank = s.chrOffsets[idx] <- chrOffset chr bank


        // CHR Banks
        // CHR map mode $8000.D7 = 0 	$8000.D7 = 1
        // PPU Bank 	Value of MMC3 register
        // $0000-$03FF 	R0 	R2
        // $0400-$07FF 	    R3
        // $0800-$0BFF 	R1 	R4
        // $0C00-$0FFF 	    R5
        // $1000-$13FF 	R2 	R0
        // $1400-$17FF 	R3
        // $1800-$1BFF 	R4 	R1
        // $1C00-$1FFF 	R5 
        if not chrMode then
            // 2KB
            setChr 0 r0; setChr 1 (r0 + 1)
            setChr 2 r1; setChr 3 (r1 + 1)
            // 1KB
            setChr 4 r2
            setChr 5 r3
            setChr 6 r4
            setChr 7 r5
        else
            // 1KB
            setChr 0 r2
            setChr 1 r3
            setChr 2 r4
            setChr 3 r5
            // 2KB
            setChr 4 r0; setChr 5 (r0 + 1)
            setChr 6 r1; setChr 7 (r1 + 1)

        // PRG Banks
        // Bit 6 of the last value written to $8000 swaps the PRG windows at $8000 and $C000. The MMC3 uses one map if bit 6 was cleared to 0 (value & $40 == $00) and another if set to 1 (value & $40 == $40).
        // PRG map mode → 	$8000.D6 = 0 	$8000.D6 = 1
        // CPU Bank 	Value of MMC3 register
        // $8000-$9FFF 	R6   (-2)
        // $A000-$BFFF 	R7    R7
        // $C000-$DFFF 	(-2)  R6
        // $E000-$FFFF 	(-1) (-1) 
        let r6 = int s.bankRegs[6]
        let r7 = int s.bankRegs[7]
        let lastBank = prg.Length / 0x2000 - 1

        if not prgMode then
            s.prgOffsets[0] <- prgOffset prg r6
            s.prgOffsets[1] <- prgOffset prg r7
            s.prgOffsets[2] <- prgOffset prg (lastBank - 1)
            s.prgOffsets[3] <- prgOffset prg lastBank
        else
            s.prgOffsets[0] <- prgOffset prg (lastBank - 1)
            s.prgOffsets[1] <- prgOffset prg r7
            s.prgOffsets[2] <- prgOffset prg r6
            s.prgOffsets[3] <- prgOffset prg lastBank

    let init (prg : byte[]) (chr : byte[]) hardwiredMirroring =
        let s = {
            mirroring = hardwiredMirroring
            hardwiredMirroring = hardwiredMirroring
            bankSelect = 0uy
            bankRegs = Array.zeroCreate<byte> 8
            prgOffsets = Array.zeroCreate<int> 4
            chrOffsets = Array.zeroCreate<int> 8
            irqReloadValue = 0uy
            irqCounter = 0uy
            irqReload = false
            irqEnabled = false
            irqPending = false
            prgRam = Array.zeroCreate 0x2000
            a12LastHigh = false
            a12LowCycles = 0
            lastPpuTick = -1
            isSaved = false
        }
        
        // 初期状態はモード 0 の配置
        s.bankRegs[0] <- 0uy
        s.bankRegs[1] <- 2uy
        s.bankRegs[2] <- 4uy
        s.bankRegs[3] <- 5uy
        s.bankRegs[4] <- 6uy
        s.bankRegs[5] <- 7uy
        s.bankRegs[6] <- 0uy
        s.bankRegs[7] <- 1uy

        calcBanks prg chr s
        s

    // ---- CPU ----

    let cpuRead addr (prg : byte[]) state =
        let bankIndex = (addr - 0x8000) >>> 13
        let offset = state.prgOffsets[bankIndex] + (addr &&& 0x1FFF)
        prg[offset]

    let cpuWrite addr value (prg : byte[]) (chr : byte[]) s =
        match addr &&& 0xE001 with
        | 0x8000 ->
            s.bankSelect <- value
            calcBanks prg chr s
        | 0x8001 ->
            let target = s.bankSelect &&& 0b0000_0111uy |> int
            s.bankRegs[target] <- value
            calcBanks prg chr s
        | 0xA000 ->
            // 4-screen VRAM 配線の場合はミラーリングビットを無視
            s.mirroring <-
                if s.hardwiredMirroring = FourScreen then
                    s.mirroring
                else
                    if value &&& 1uy <> 0uy then Horizontal else Vertical
        | 0xA001 ->
            () // PRG RAM の保護機能
        | 0xC000 ->
            s.irqReloadValue <- value
        | 0xC001 ->
            s.irqCounter <- 0uy
            s.irqReload <- true
        | 0xE000 ->
            s.irqEnabled <- false
            s.irqPending <- false
        | 0xE001 ->
            s.irqEnabled <- true
        | _ -> ()

        s

    // ---- PPU ----

    let ppuRead addr (chr : byte[]) state =
        if addr >= 0x2000 then
            0uy
        else
            let bankIndex = addr >>> 10
            let offset = state.chrOffsets[bankIndex] + (addr &&& 0x3FF)
            chr[offset]

    let ppuWrite addr value (chrRam : byte[]) state =
        if addr >= 0x2000 then
            ()
        else
            let bankIndex = addr >>> 10
            let offset = state.chrOffsets[bankIndex] + (addr &&& 0x3FF)
            chrRam[offset] <- value

    let basePrgRamAddr = 0x6000

    let readPrgRam addr state =
        state.prgRam[addr - basePrgRamAddr]

    let writePrgRam addr value state =
        state.prgRam[addr - basePrgRamAddr] <- value
        state.isSaved <- true
        state

    let setPrgRam data state =
        Array.blit data 0 state.prgRam 0 data.Length

    let pollIrq state = state.irqPending

    let getMirroring state = state.mirroring
