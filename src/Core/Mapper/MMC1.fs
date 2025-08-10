namespace HamicomEmu.Mapper

module MMC1 =

    open HamicomEmu.Mapper.Common

    type NametableArrangement =
    | OneScreenLower // 0: $000
    | OneScreenUpper // 1: $400
    | Vertical // 2
    | Horizontal // 3

    type PrgBankMode =
    | Mode32K // 0-1
    | FixFirstBank // 2
    | FixLastBank // 3

    type ChrBankMode =
    | Mode8K // 0
    | Mode4K // 1

    /// TODO:
    /// MMC1A の場合は常に PRG RAM を有効化
    /// MMC1B の場合は $E000 - の bit 4 をセットすることで無効化できる
    /// MMC1A には使われていなかったバイパスロジックも存在する（MMC1B には無い）
    /// https://www.nesdev.org/wiki/MMC1#ASIC_Revisions
    type State = {
        shiftReg: byte
        shiftCount: int
        nametableSwitch: NametableArrangement
        prgBankMode: PrgBankMode
        chrBankMode: ChrBankMode
        chrBank0: byte
        chrBank1: byte
        prgBank: byte
        prgRam: byte[]
        prgRamDisable: bool
    }

    let init = {
        shiftReg = 0x10uy
        shiftCount = 0
        nametableSwitch = OneScreenLower
        prgBankMode = FixLastBank
        chrBankMode = Mode8K
        chrBank0 = 0uy
        chrBank1 = 0uy
        prgBank = 0uy
        prgRam = Array.create 0x2000 0uy
        prgRamDisable = false
    }

    let resetShiftAndCount = 0x10uy, 0

    let parseChrBankMode v =
        match int v &&& 0b1_0000 >>> 4 with
        | 0 -> Mode8K
        | _ -> Mode4K

    let parsePrgBankMode v =
        match int v &&& 0b0_1100 >>> 2 with
        | 2 -> FixFirstBank
        | 3 -> FixLastBank
        | _ -> Mode32K

    let parseNametableSwitch v =
        match int v &&& 0b0_0011 with
        | 0 -> OneScreenLower
        | 1 -> OneScreenUpper
        | 2 -> Vertical
        | _ -> Horizontal

    let getMirroring state =
        match state.nametableSwitch with
        | Vertical -> Some Mirroring.Vertical
        | Horizontal -> Some Mirroring.Horizontal
        | _ -> None // とりあえずの処置

    let basePrgAddr = 0x8000
    let prgBankSize mode =
        match mode with
        | Mode32K -> 32 * 1024
        | FixFirstBank
        | FixLastBank -> 16 * 1024
    
    let chrBankSize mode =
        match mode with
        | Mode8K -> 8 * 1024
        | Mode4K -> 4 * 1024

    let basePrgRamAddr = 0x6000

    let readPrgRam addr state =
        state.prgRam[addr - basePrgRamAddr]

    let writePrgRam addr value state =
        state.prgRam[addr - basePrgRamAddr] <- value
        state

    let setPrgRam data state =
        Array.blit data 0 state.prgRam 0 data.Length

    let cpuRead addr (prg: byte[]) state =
        let mode = state.prgBankMode
        let bankSize = prgBankSize mode
        let totalBanks = prg.Length / bankSize

        match mode with
        | Mode32K ->
            let bank = state.prgBank &&& 0x1Euy
            let offset = getOffset (int bank % totalBanks) bankSize basePrgAddr
            prg[addr + offset]

        | FixFirstBank ->
            if addr >= 0x8000 && addr <= 0xBFFF then
                prg[addr - basePrgAddr]

            elif addr >= 0xC000 && addr <= 0xFFFF then
                let bank = state.prgBank &&& 0x1Fuy
                let offset = getOffset (int bank % totalBanks) bankSize 0xC000
                prg[addr + offset]
            else
                printfn "[MMC1 PRG READ] Invalid address: %04X" addr
                0uy

        | FixLastBank ->
            if addr >= 0x8000 && addr <= 0xBFFF then
                let bank = state.prgBank &&& 0x1Fuy
                let offset = getOffset (int bank % totalBanks) bankSize basePrgAddr
                prg[addr + offset]

            elif addr >= 0xC000 && addr <= 0xFFFF then
                let offset = getOffset (totalBanks - 1) bankSize 0xC000
                prg[addr + offset]
            else
                printfn "[MMC1 PRG READ] Invalid address: %04X" addr
                0uy

    let ppuRead addr (chr: byte[]) state =
        let mode = state.chrBankMode
        let bankSize = chrBankSize mode
        let totalBanks = chr.Length / bankSize
        match mode with
        | Mode8K ->
            let bank = int state.chrBank0 &&& 0x1E
            let offset = getOffset (bank % totalBanks) bankSize 0
            chr[addr + offset]

        | Mode4K ->
            match addr &&& 0x1000 with
            | 0x0000 ->
                let bank = int state.chrBank0 &&& 0x1F
                let offset = getOffset (bank % totalBanks) bankSize 0
                chr[addr + offset]
            | 0x1000 ->
                let bank = int state.chrBank1 &&& 0x1F
                let offset = getOffset (totalBanks - 1) bankSize 0x1000
                chr[addr + offset]
            | _ ->
                printfn "[MMC1 CHR READ] Invalid address: %04X" addr
                0uy


    let cpuWrite addr value state =
        let mutable shiftReg = state.shiftReg
        let mutable shiftCount = state.shiftCount
        let mutable nametableSwitch = state.nametableSwitch
        let mutable prgBankMode = state.prgBankMode
        let mutable chrBankMode = state.chrBankMode
        let mutable chrBank0 = state.chrBank0
        let mutable chrBank1 = state.chrBank1
        let mutable prgBank = state.prgBank

        if value &&& 0x80uy <> 0uy then
            // 最上位ビットがセットされている場合
            // シフトレジスタをリセットしてバンクモードを最後尾固定する
            let sr, sc = resetShiftAndCount
            shiftReg <- sr
            shiftCount <- sc
            prgBankMode <- FixLastBank

        else
            let bit = value &&& 0x01uy

            shiftReg <- shiftReg >>> 1
            shiftReg <- shiftReg ||| (bit <<< 4)
            shiftCount <- shiftCount + 1

            if shiftCount >= 5 then
                if addr >= 0x8000 && addr <= 0x9FFF then
                    nametableSwitch <- parseNametableSwitch shiftReg
                    prgBankMode <- parsePrgBankMode shiftReg
                    chrBankMode <- parseChrBankMode shiftReg
                elif addr >= 0xA000 && addr <= 0xBFFF then
                    chrBank0 <- shiftReg
                elif addr >= 0xC000 && addr <= 0xDFFF then
                    chrBank1 <- shiftReg
                elif addr >= 0xE000 && addr <= 0xFFFF then
                    prgBank <- shiftReg

                let sr, sc = resetShiftAndCount
                shiftReg <- sr
                shiftCount <- sc

        {
            state with
                shiftReg = shiftReg
                shiftCount = shiftCount
                nametableSwitch = nametableSwitch
                prgBankMode = prgBankMode
                chrBankMode = chrBankMode
                chrBank0 = chrBank0
                chrBank1 = chrBank1
                prgBank = prgBank
        }
