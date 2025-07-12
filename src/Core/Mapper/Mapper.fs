namespace HamicomEmu.Mapper

module Mapper =

    open HamicomEmu.Mapper.Types
    open HamicomEmu.Cartridge

    let getMirroring defaultMirroring mapper =
        match mapper with
        | VRC1 state ->
            match defaultMirroring, state.mirroring with
            | FourScreen, _ -> defaultMirroring // 4 画面の時はマッパーを無視
            | _, Vertical -> Vertical
            | _, Horizontal -> Horizontal
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

    let getOffset calcBank bankSize baseAddr = calcBank * bankSize - baseAddr

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
            let offset = getOffset (int state.prgSelect0 % totalBanks) bankSize 0x8000
            prg[addr + offset]

        elif addr >= 0xA000 && addr < 0xC000 then
            let offset = getOffset (int state.prgSelect1 % totalBanks) bankSize 0xA000
            prg[addr + offset]

        elif addr >= 0xC000 && addr < 0xE000 then
            let offset = getOffset (int state.prgSelect2 % totalBanks) bankSize 0xC000
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
            { state with prgSelect0 = bank }

        // ミラーリングと CHR バンク 0, 1 の上位ビット
        elif addr >= 0x9000 && addr < 0xA000 then
            let mirror = if value &&& 1uy <> 0uy then Horizontal else Vertical
            let select0Hi = (value &&& 0b0010uy) <<< 3
            let select1Hi = (value &&& 0b0100uy) <<< 2
            let chrSelect0 = (state.chrSelect0 &&& 0x0Fuy) ||| select0Hi
            let chrSelect1 = (state.chrSelect1 &&& 0x0Fuy) ||| select1Hi
            // printfn "[VRC WRITE CHR HI] chrSel0: %04X chrSel1: %04X, Hi0: %02X Hi1: %02X" chrSelect0 chrSelect1 select0Hi select1Hi
            {
                state with
                    mirroring = mirror
                    chrSelect0 = chrSelect0
                    chrSelect1 = chrSelect1
            }

        // PRG バンク設定 1
        elif addr >= 0xA000 && addr < 0xC000 then
            let bank = value &&& 0x0Fuy
            { state with prgSelect1 = bank }

        // PRG バンク設定 2
        elif addr >= 0xC000 && addr < 0xE000 then
            let bank = value &&& 0x0Fuy
            { state with prgSelect2 = bank }

        // CHR バンク 0 の下位ビット
        elif addr >= 0xE000 && addr < 0xF000 then
            let lo = value &&& 0x0Fuy
            let v = (state.chrSelect0 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel0: %04X, Lo: %02X" v lo
            { state with chrSelect0 = v }

        // CHR バンク 1 の下位ビット
        elif addr >= 0xF000 then
            let lo = value &&& 0x0Fuy
            let v = (state.chrSelect1 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel1: %04X Lo: %02X" v lo
            { state with chrSelect1 = v }

        else
            printfn "[MAPPER VRC1 WRITE PRG] Invalid address: %04X" addr
            state

    let cpuRead (addr: uint16) cart =
        let addr = int addr

        match cart.mapper with
        | UxROM state -> readUxrom addr cart state

        | VRC1 state -> readPrgVRC1 addr cart state

        | NROM _
        | _ -> readNrom addr cart

    let cpuWrite (addr: uint16) value cart =
        let addr = int addr

        match cart.mapper with
        | UxROM _ when addr >= 0x8000 ->
            // bank 選択
            let newState = { bankSelect = value &&& 0x0Fuy }
            UxROM newState, ()

        | VRC1 state ->
            let newState = writePrgVRC1 addr value state
            VRC1 newState, ()

        | J87 _ when addr >= 0x6000 && addr <= 0x7FFF ->
            let newState = { bankSelect = value &&& 0b11uy } // 0-1 bits 使用
            // printfn "[MAPPER CPU WRITE] Bank Select: %A" newState.bankSelect
            J87 newState, ()

        | _ ->
            printfn "Attempt to write to Cartridge Rom space. addr: %04X" addr
            cart.mapper, ()

    /// バンクサイズは 4 KB
    let getChrAddressVRC1 addr cart state =
        let chr = cart.chrRom
        let select0 = int state.chrSelect0
        let select1 = int state.chrSelect1
        let bankSize = 4 * 1024
        let totalBanks = chr.Length / bankSize

        if addr < 0x1000 then
            let offset = getOffset (select0 % totalBanks) bankSize 0
            addr + offset

        else
            let offset = getOffset (select1 % totalBanks) bankSize 0x1000
            addr + offset

    let ppuRead addr cart mapper =
        let addr = int addr

        match mapper with
        | VRC1 state ->
            let addr' = getChrAddressVRC1 addr cart state
            cart.chrRom[addr']

        | J87 state ->
            let chr = cart.chrRom
            let hi = state.bankSelect &&& 0b01uy
            let lo = state.bankSelect &&& 0b10uy
            let bitsCorrected = ((hi <<< 1) ||| (lo >>> 1)) |> int
            let bankSize = 8 * 1024
            let totalBanks = chr.Length / bankSize
            let bank = bitsCorrected % totalBanks
            let offset = bank * bankSize
            chr[addr + offset]

        | NROM _
        | _ when cart.chrRom <> [||] -> cart.chrRom[addr]

        | NROM _
        | _ -> cart.chrRam[addr]

    let ppuReadRange startAddr endAddr cart mapper =
        match cart.mapper with
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
