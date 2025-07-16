namespace HamicomEmu.Mapper

/// TODO: マッパーごとにファイル分割
/// TODO: バス競合の再現 https://www.nesdev.org/wiki/Bus_conflict
module Mapper =

    open HamicomEmu.Mapper.Types
    open HamicomEmu.Mapper.Common
    open HamicomEmu.Cartridge

    /// TODO: もう少し見通しをよくしたい…ミラーリング型を MapperCommons で定義する？
    let getMirroring defaultMirroring mapper =
        match defaultMirroring with
        | FourScreen -> defaultMirroring // 4 画面の時はマッパーを無視
        | _ ->
            match mapper with
            | MMC1 state ->
                match MMC1.getMirroring state with
                | MMC1.NametableArrangement.Vertical -> Vertical
                | MMC1.NametableArrangement.Horizontal -> Horizontal
                | _ -> Vertical

            | VRC1 state ->
                match state.mirroring with
                | Vertical -> Vertical
                | Horizontal -> Horizontal
                | _ -> defaultMirroring
            | _ -> defaultMirroring

    let readNrom addr cart = // PRG ROM の読み込み
        let addr' = addr - 0x8000 // 0x8000 - 0xFFFF の範囲を 0x0000 - 0x7FFF に変換

        // 16KB ROM の場合はミラーリング
        let addr2 =
            if cart.prgRom.Length = 0x4000 && addr' >= 0x4000 then
                addr' % 0x4000
            else
                addr'

        cart.prgRom[addr2]

    let readUxrom addr cart state =
        let prg = cart.prgRom
        let bankSize = 16 * 1024 // 16 KB
        let totalBanks = prg.Length / bankSize

        if addr >= 0x8000 && addr < 0xC000 then
            // 可変バンク
            let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0x8000
            prg[addr + offset]

        elif addr >= 0xC000 then
            // 固定バンク（最後尾）
            let offset = getOffset (totalBanks - 1) bankSize 0xC000
            prg[addr + offset]

        else
            // PRG-ROM の外参照
            0uy

    let readPrgVRC1 addr cart state =
        let prg = cart.prgRom
        let bankSize = 8 * 1024 // 8 KB
        let totalBanks = prg.Length / bankSize

        // ここから可変バンク
        if addr >= 0x8000 && addr < 0xA000 then
            let offset = getOffset (int state.prgBank0 % totalBanks) bankSize 0x8000
            prg[addr + offset]

        elif addr >= 0xA000 && addr < 0xC000 then
            let offset = getOffset (int state.prgBank1 % totalBanks) bankSize 0xA000
            prg[addr + offset]

        elif addr >= 0xC000 && addr < 0xE000 then
            let offset = getOffset (int state.prgBank2 % totalBanks) bankSize 0xC000
            prg[addr + offset]

        // 固定バンク
        elif addr >= 0xE000 then
            let offset = getOffset (totalBanks - 1) bankSize 0xE000
            prg[addr + offset]

        else
            printfn "[MAPPER VRC1 READ PRG] Invalid address: %04X" addr
            0uy

    let writePrgVRC1 addr value state =
        // PRG バンク設定 0
        if addr >= 0x8000 && addr < 0x9000 then
            let bank = value &&& 0x0Fuy
            { state with prgBank0 = bank }

        // ミラーリングと CHR バンク 0, 1 の上位ビット
        elif addr >= 0x9000 && addr < 0xA000 then
            let mirror = if value &&& 1uy <> 0uy then Horizontal else Vertical
            let bank0Hi = (value &&& 0b0010uy) <<< 3
            let bank1Hi = (value &&& 0b0100uy) <<< 2
            let chrBank0 = (state.chrBank0 &&& 0x0Fuy) ||| bank0Hi
            let chrBank1 = (state.chrBank1 &&& 0x0Fuy) ||| bank1Hi
            {
                state with
                    mirroring = mirror
                    chrBank0 = chrBank0
                    chrBank1 = chrBank1
            }

        // PRG バンク設定 1
        elif addr >= 0xA000 && addr < 0xC000 then
            let bank = value &&& 0x0Fuy
            { state with prgBank1 = bank }

        // PRG バンク設定 2
        elif addr >= 0xC000 && addr < 0xE000 then
            let bank = value &&& 0x0Fuy
            { state with prgBank2 = bank }

        // CHR バンク 0 の下位ビット
        elif addr >= 0xE000 && addr < 0xF000 then
            let lo = value &&& 0x0Fuy
            let v = (state.chrBank0 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel0: %04X, Lo: %02X" v lo
            { state with chrBank0 = v }

        // CHR バンク 1 の下位ビット
        elif addr >= 0xF000 then
            let lo = value &&& 0x0Fuy
            let v = (state.chrBank1 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel1: %04X Lo: %02X" v lo
            { state with chrBank1 = v }

        else
            printfn "[MAPPER VRC1 WRITE PRG] Invalid address: %04X" addr
            state

    let readPrgRam (addr: uint16) cart =
        let addr = int addr
        match cart.mapper with
        | MMC1 state ->
            MMC1.readPrgRam addr state
        | _ ->
            printfn "[MAPPER %A] read from PRG Ram is not supported." cart.mapper
            0uy
    
    let writePrgRam (addr: uint16) value cart =
        let addr = int addr
        match cart.mapper with
        | MMC1 state ->
            let newState = MMC1.writePrgRam addr value state
            MMC1 newState, ()
        | _ ->
            printfn "[MAPPER %A] write to PRG Ram is not supported." cart.mapper
            cart.mapper, ()

    let cpuRead (addr: uint16) cart =
        let addr = int addr
        let prg = cart.prgRom

        match cart.mapper with
        | MMC1 state ->
            MMC1.cpuRead addr prg state

        | UxROM state -> readUxrom addr cart state

        | VRC1 state -> readPrgVRC1 addr cart state

        | NROM _
        | _ -> readNrom addr cart

    let cpuWrite (addr: uint16) value cart =
        let addr = int addr

        match cart.mapper with
        | MMC1 state ->
            let newState = MMC1.cpuWrite addr value state
            MMC1 newState, ()

        | UxROM _ when addr >= 0x8000 ->
            // bank 選択
            let newState = { bankSelect = value &&& 0x0Fuy }
            UxROM newState, ()

        | CNROM _ when addr >= 0x8000 ->
            // TODO: マスクに関しては今後要確認
            let romSignal = cart.prgRom[addr - 0x8000]
            let v = value &&& romSignal
            let newState = { bankSelect = v }
            CNROM newState, ()

        | VRC1 state ->
            let newState = writePrgVRC1 addr value state
            VRC1 newState, ()

        | J87 _ when addr >= 0x6000 && addr <= 0x7FFF ->
            let hi = value &&& 0b01uy <<< 1
            let lo = value &&& 0b10uy >>> 1
            let v = hi ||| lo
            let newState = { bankSelect = v &&& 0b11uy } // 0-1 bits 使用
            // printfn "[MAPPER CPU WRITE] Bank Select: %A" newState.bankSelect
            J87 newState, ()

        | _ ->
            printfn "Attempt to write to Cartridge Rom space. addr: %04X" addr
            cart.mapper, ()

    let getChrAddressCnrom addr cart state =
        let chr = cart.chrRom
        let bankSize = 8 * 1024
        let totalBanks = chr.Length / bankSize

        let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0
        addr + offset

    /// バンクサイズは 4 KB
    let getChrAddressVRC1 addr cart state =
        let chr = cart.chrRom
        let select0 = int state.chrBank0
        let select1 = int state.chrBank1
        let bankSize = 4 * 1024
        let totalBanks = chr.Length / bankSize

        if addr < 0x1000 then
            let offset = getOffset (select0 % totalBanks) bankSize 0
            addr + offset

        else
            let offset = getOffset (select1 % totalBanks) bankSize 0x1000
            addr + offset

    let getChrAddressJ87 addr cart state =
        let chr = cart.chrRom
        let bankSize = 8 * 1024
        let totalBanks = chr.Length / bankSize

        let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0
        addr + offset

    let getChrMem cart =
        if cart.chrRom <> [||] then
            cart.chrRom
        else
            cart.chrRam

    let ppuRead addr cart mapper =
        let addr = int addr

        match mapper with
        | MMC1 state ->
            let data = MMC1.ppuRead addr (getChrMem cart) state
            data

        | CNROM state ->
            let addr' = getChrAddressCnrom addr cart state
            cart.chrRom[addr']

        | VRC1 state ->
            let addr' = getChrAddressVRC1 addr cart state
            cart.chrRom[addr']

        | J87 state ->
            let addr' = getChrAddressJ87 addr cart state
            cart.chrRom[addr']

        | NROM _
        | _ ->
            let chr = getChrMem cart
            chr[addr]

    let ppuReadRange startAddr endAddr cart mapper =
        match cart.mapper with
        | MMC1 _
        | CNROM _
        | VRC1 _
        | J87 _ -> Array.init (endAddr - startAddr + 1) (fun i -> ppuRead (startAddr + i) cart mapper)

        | NROM _
        | _ when cart.chrRom <> [||] -> cart.chrRom[startAddr..endAddr]

        | NROM _
        | _ -> cart.chrRam[startAddr..endAddr]

    let ppuWrite addr value cart =
        let addr = int addr

        match cart.mapper with
        | NROM _
        | _ when cart.chrRom <> [||] -> cart

        | NROM _
        | _ ->
            cart.chrRam[int addr] <- value
            cart
