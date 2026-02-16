namespace HamicomEmu.Mapper

module VRC1 =

    open HamicomEmu.Mapper.Common

    type State = {
        prgBank0: byte
        prgBank1: byte
        prgBank2: byte
        mirroring: Mirroring
        chrBank0: byte
        chrBank1: byte
    }

    let init = {
        prgBank0 = 0uy
        prgBank1 = 0uy
        prgBank2 = 0uy
        mirroring = Horizontal
        chrBank0 = 0uy
        chrBank1 = 0uy
    }

    let getMirroring state =
        match state.mirroring with
        | Vertical -> Some Vertical
        | Horizontal -> Some Horizontal
        | _ -> None

    let cpuRead addr (prg: byte[]) state =
        let bankSize = 8 * 1024 // 8 KB
        let totalBanks = prg.Length / bankSize

        // ここから可変バンク
        match addr &&& 0xE000 with
        | 0x8000 ->
            let offset = getOffset (int state.prgBank0 % totalBanks) bankSize 0x8000
            prg[addr + offset]

        | 0xA000 ->
            let offset = getOffset (int state.prgBank1 % totalBanks) bankSize 0xA000
            prg[addr + offset]

        | 0xC000 ->
            let offset = getOffset (int state.prgBank2 % totalBanks) bankSize 0xC000
            prg[addr + offset]

        // 固定バンク
        | 0xE000 ->
            let offset = getOffset (totalBanks - 1) bankSize 0xE000
            prg[addr + offset]

        | _ ->
            printfn "[MAPPER VRC1 READ PRG] Invalid address: %04X" addr
            0uy

    let cpuWrite addr value state =
        // PRG バンク設定 0
        match addr &&& 0xF000 with
        | 0x8000 ->
            let bank = value &&& 0x0Fuy
            { state with prgBank0 = bank }

        // ミラーリングと CHR バンク 0, 1 の上位ビット
        | 0x9000 ->
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
        | 0xA000 | 0xB000 ->
            let bank = value &&& 0x0Fuy
            { state with prgBank1 = bank }

        // PRG バンク設定 2
        | 0xC000 | 0xD000 ->
            let bank = value &&& 0x0Fuy
            { state with prgBank2 = bank }

        // CHR バンク 0 の下位ビット
        | 0xE000 ->
            let lo = value &&& 0x0Fuy
            let v = (state.chrBank0 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel0: %04X, Lo: %02X" v lo
            { state with chrBank0 = v }

        // CHR バンク 1 の下位ビット
        | 0xF000 ->
            let lo = value &&& 0x0Fuy
            let v = (state.chrBank1 &&& 0xF0uy) ||| lo
            // printfn "[VRC WRITE CHR LO] chrSel1: %04X Lo: %02X" v lo
            { state with chrBank1 = v }

        | _ ->
            printfn "[MAPPER VRC1 WRITE PRG] Invalid address: %04X" addr
            state
    /// バンクサイズは 4 KB
    let ppuRead addr (chr: byte[]) state =
        let select0 = int state.chrBank0
        let select1 = int state.chrBank1
        let bankSize = 4 * 1024
        let totalBanks = chr.Length / bankSize

        if addr < 0x1000 then
            let offset = getOffset (select0 % totalBanks) bankSize 0
            chr[addr + offset]

        else
            let offset = getOffset (select1 % totalBanks) bankSize 0x1000
            chr[addr + offset]
