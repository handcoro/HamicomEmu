namespace HamicomEmu.Mapper

module Namco163 =

    open HamicomEmu.Mapper.Common

    type ChrMode =
    | Chr
    | NameTable

    type State = {
        mutable internalRam: byte array // TODO: 音源モジュールとして分割する
        mutable prgRam: byte array
        mutable loChrNtMode: bool
        mutable hiChrNtMode: bool
        prgBankSelects: byte array
        chrBankSelects: (byte * ChrMode) array
        mutable internalRamAddress: int
        wramWriteProtectControl: byte
        externalRamWriteProtect: bool
        mutable irqCounter: int
        disableSound: bool
        mutable isSaved: bool
    }

    let internalRamSize = 0x80

    let init = {
        internalRam = Array.zeroCreate internalRamSize
        prgRam = Array.zeroCreate 0x2000
        loChrNtMode = false
        hiChrNtMode = false
        prgBankSelects = Array.zeroCreate 3
        chrBankSelects = Array.create 12 (0uy, Chr)
        internalRamAddress = 0
        wramWriteProtectControl = 0uy
        externalRamWriteProtect = false
        irqCounter = 0
        disableSound = false
        isSaved = false
    }

    let internalRamAddressMasked state = state.internalRamAddress &&& 0x7F

    let internalRamAddressIncrement state =
        let autoIncrement = state.internalRamAddress &&& 0x80 <> 0
        let masked = internalRamAddressMasked state
        if autoIncrement && masked <> 0x7F then
            state.internalRamAddress <- state.internalRamAddress + 1
        else
            ()

    // TODO: セーブデータの大きさでデータの振り分けを決める
    // 読み込んだデータのセーブ判定フラグをセット
    let setDataToRam data state =
        let iRam, prgRam = Array.splitAt internalRamSize data
        state.internalRam <- iRam
        state.prgRam <- prgRam

    // TODO: セーブ判定を内部/外部 RAM で個別でやり不要なら省いてデータを作成する
    // let ir = if state.isSavedInternalRam then Some (state.internalRam) else None
    // let pr = if state.isSavedPrgRam then Some (state.prgRam) else None
    // match ...
    let getDataFromRam state =
        Array.append state.internalRam state.prgRam

    let readInternalRam state =
        let before = internalRamAddressMasked state
        internalRamAddressIncrement state
        state.internalRam[before], state

    let writeInternalRam value state =
        let before = internalRamAddressMasked state
        internalRamAddressIncrement state
        state.internalRam[before] <- value
        state.isSaved <- true
        state

    let basePrgRamAddr = 0x6000

    let readPrgRam addr state =
        state.prgRam[addr - basePrgRamAddr]

    let writePrgRam addr value state =
        state.prgRam[addr - basePrgRamAddr] <- value
        state.isSaved <- true
        state

    let incrementIrqCounter state =
        let counter = state.irqCounter &&& 0xFFFF
        let enabled = counter &&& 0x8000 <> 0
        if enabled && counter &&& 0x7FFF <> 0x7FFF then
            state.irqCounter <- (counter + 1) &&& 0xFFFF
        else
            ()

    let pollIrq state =
        let counter = state.irqCounter &&& 0xFFFF
        if counter = 0x7FFF then
            true
        else
            false

    let cpuRead addr (prg : byte[]) state =
        let bankSize = 1024 * 8 // 8 KB
        let totalBanks = prg.Length / bankSize
        match addr &&& 0xF800 with
        // IRQ
        | 0x5000 ->
            state.irqCounter &&& 0xFF |> byte
        | 0x5800 ->
            state.irqCounter >>> 8 |> byte

        // 最後尾バンク固定
        | 0xE000 | 0xE800 | 0xF000 | 0xF800 ->
            let offset = getOffset (totalBanks - 1) bankSize 0xE000
            prg[addr + offset]
        | _ ->
            let bankIndex = (addr - 0x8000) >>> 13
            let bank = int state.prgBankSelects[bankIndex]
            let offset = getOffset (bank % totalBanks) bankSize (0x8000 + bankIndex * 0x2000)
            prg[addr + offset]

    let cpuWrite addr value state =

        match addr &&& 0xF800 with
        // IRQ
        | 0x5000 ->
            state.irqCounter <- state.irqCounter &&& 0xFF00 ||| int value
        | 0x5800 ->
            state.irqCounter <- state.irqCounter &&& 0x00FF ||| (int value <<< 8)
        // 以下バンク選択設定
        // 0-3 CHR
        | 0x8000 | 0x8800 | 0x9000 | 0x9800 ->
            let bankIndex = addr - 0x8000 >>> 11
            let v =
                if not state.loChrNtMode && value >= 0xE0uy then
                    value &&& 1uy, NameTable // 0: A, 1: B
                else
                    value, Chr
            state.chrBankSelects[bankIndex] <- v

        // 4-7 CHR
        | 0xA000 | 0xA800 | 0xB000 | 0xB800 ->
            let bankIndex = (addr - 0xA000 >>> 11) + 4
            let v =
                if not state.hiChrNtMode && value >= 0xE0uy then
                    value &&& 1uy, NameTable
                else
                    value, Chr
            state.chrBankSelects[bankIndex] <- v

        // 8-11 CHR
        | 0xC000 | 0xC800 | 0xD000 | 0xD800 ->
            let bankIndex = (addr - 0xC000 >>> 11) + 8
            let v =
                if value >= 0xE0uy then
                    value &&& 1uy, NameTable
                else
                    value, Chr
            state.chrBankSelects[bankIndex] <- v

        // bit 0-5 PRG
        // TODO: 音声制御
        | 0xE000 ->
            state.prgBankSelects[0] <- value &&& 0b0011_1111uy

        // bit 0-5 PRG
        // bit 6/7 0: CHR-ROM/RAM, 1: CHR 選択設定時に値が >= 0xE0 なら VRAM
        | 0xE800 ->
            state.loChrNtMode <- value &&& 0b0100_0000uy <> 0uy
            state.hiChrNtMode <- value &&& 0b1000_0000uy <> 0uy
            state.prgBankSelects[1] <- value &&& 0b0011_1111uy

        // bit 0-5 PRG
        | 0xF000 ->
            state.prgBankSelects[2] <- value &&& 0b0011_1111uy

        // 内部 RAM アドレスの指定
        // TODO: 外部 RAM 書き込みプロテクトの実装
        | 0xF800 ->
            state.internalRamAddress <- int value

        | _ ->
            ()

        state

    let inline getChrPageIndex addr = addr >>> 10

    let inline getChrPageMode state idx = snd state.chrBankSelects[idx]

    let getPpuAddress addr (chr : byte[]) state =
        let bankSize = 1024 * 1 // 1 KB
        let totalBanks = chr.Length / bankSize
        let idx = getChrPageIndex addr

        let page = fst state.chrBankSelects[idx]
        let bank = int page % totalBanks 
        let offset = getOffset bank bankSize 0
        let res = offset + (addr &&& 0x3FF)
        res

    let ppuRead addr (chr : byte[]) (vram : byte[]) state =
        let mode =
            addr
            |> getChrPageIndex
            |> getChrPageMode state

        let res = getPpuAddress addr chr state

        if mode = NameTable then
            vram[res]
        else
            chr[res]

    // ネームテーブルの場合は VRAM 書き換え
    let ppuWrite addr value (chr : byte[]) (vram : byte[]) state =
        let mode =
            addr
            |> getChrPageIndex
            |> getChrPageMode state

        if mode = NameTable then
            let res = getPpuAddress addr chr state
            vram[res] <- value
        else
            ()
