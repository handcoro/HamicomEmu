namespace HamicomEmu.Mapper

/// TODO: マッパーごとにファイル分割
/// TODO: バス競合の再現 https://www.nesdev.org/wiki/Bus_conflict
module Mapper =

    open HamicomEmu.Mapper.Types
    open HamicomEmu.Mapper.Common
    open HamicomEmu.Cartridge

    let getMirroring defaultMirroring mapper =
        match defaultMirroring with
        | FourScreen -> defaultMirroring // 4 画面の時はマッパーを無視
        | _ ->
            match mapper with
            | MMC1 state ->
                match MMC1.getMirroring state with
                | Some mirror -> mirror
                | None -> defaultMirroring
            
            | MMC3 state ->
                Mmc3.getMirroring state

            | Namco163 _ ->
                FourScreen // マッパーが 4 画面分割り当てる仕様

            | VRC1 state ->
                match VRC1.getMirroring state with
                | Some mirror -> mirror
                | None -> defaultMirroring
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

    let readInternalRam (addr : uint16) cart =
        match cart.mapper with
        | Namco163 state ->
            let data, s = Namco163.readInternalRam state
            data, Namco163 s
        | _ ->
            0uy, cart.mapper

    let writeInternalRam (addr : uint16) value cart =
        match cart.mapper with
        | Namco163 state ->
            let s = Namco163.writeInternalRam value state
            Namco163 s, ()
        | _ ->
            cart.mapper, ()

    let readPrgRam (addr: uint16) cart =
        let addr = int addr
        match cart.mapper with
        | MMC1 state ->
            MMC1.readPrgRam addr state
        | MMC3 state ->
            Mmc3.readPrgRam addr state
        | Namco163 state ->
            Namco163.readPrgRam addr state
        | _ ->
            printfn "[MAPPER] read from PRG Ram is not supported."
            0uy
    
    let writePrgRam (addr: uint16) value cart =
        let addr = int addr
        match cart.mapper with
        | MMC1 state ->
            let newState = MMC1.writePrgRam addr value state
            MMC1 newState, ()
        | MMC3 state ->
            let newState = Mmc3.writePrgRam addr value state
            MMC3 newState, ()
        | Namco163 state ->
            let newState = Namco163.writePrgRam addr value state
            Namco163 newState, ()
        | J87 _ ->
            let newState = J87.writePrgRam value
            J87 newState, ()
        | _ ->
            printfn "[MAPPER] write to PRG Ram is not supported."
            cart.mapper, ()

    let getPrgRam cart =
        match cart.mapper with
        | MMC1 state when state.isSaved -> Some state.prgRam
        | MMC3 state when state.isSaved -> Some state.prgRam
        | Namco163 state when state.isSaved ->
            Some (Namco163.getDataFromRam state)
        | _ ->
            None

    let setPrgRam data cart =
        match cart.mapper with
        | MMC1 state ->
            MMC1.setPrgRam data state
            cart
        | MMC3 state ->
            Mmc3.setPrgRam data state
            cart
        | Namco163 state ->
            Namco163.setDataToRam data state
            cart
        | _ ->
            cart

    let getChrMem cart =
        if cart.chrRom <> [||] then
            cart.chrRom
        else
            cart.chrRam

    let cpuRead (addr: uint16) cart =
        let addr = int addr
        let prg = cart.prgRom

        match cart.mapper with
        | MMC1 state ->
            MMC1.cpuRead addr prg state

        | UxROM state -> Uxrom.cpuRead addr prg state

        | MMC3 state ->
            Mmc3.cpuRead addr prg state

        | Namco163 state ->
            Namco163.cpuRead addr prg state

        | GxROM state -> Gxrom.cpuRead addr prg state

        | VRC1 state -> VRC1.cpuRead addr prg state

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
            let newState = Uxrom.cpuWrite addr value
            UxROM newState, ()

        | CNROM _ when addr >= 0x8000 ->
            // TODO: マスクに関しては今後要確認
            let romSignal = cart.prgRom[addr - 0x8000]
            let v = value &&& romSignal
            let newState = { bankSelect = v }
            CNROM newState, ()
        
        | MMC3 state ->
            let newState = Mmc3.cpuWrite addr value cart.prgRom (getChrMem cart) state
            MMC3 newState, ()

        | Namco163 state ->
            let newState = Namco163.cpuWrite addr value state
            Namco163 newState, ()

        | GxROM _ when addr >= 0x8000 ->
            let newState = Gxrom.cpuWrite addr value
            GxROM newState, ()

        | VRC1 state ->
            let newState = VRC1.cpuWrite addr value state
            VRC1 newState, ()

        | _ ->
            printfn "Attempt to write to Cartridge Rom space. addr: %04X" addr
            cart.mapper, ()

    let getChrAddressCnrom addr cart state =
        let chr = cart.chrRom
        let bankSize = 8 * 1024
        let totalBanks = chr.Length / bankSize

        let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0
        addr + offset

    // TODO: Namco 163 以外の VRAM 読み出しに対応する
    let ppuRead addr (vram : byte[]) cart =
        let addr = int addr

        match cart.mapper with
        | Namco163 state ->
            let data = Namco163.ppuRead addr (getChrMem cart) vram state
            data

        | _ when addr >= 0x2000 ->
            vram[addr - 0x2000]

        | MMC1 state ->
            let data = MMC1.ppuRead addr (getChrMem cart) state
            data

        | CNROM state ->
            let addr' = getChrAddressCnrom addr cart state
            cart.chrRom[addr']
        
        | MMC3 state ->
            let data = Mmc3.ppuRead addr (getChrMem cart) state
            data

        | GxROM state ->
            let data = Gxrom.ppuRead addr (getChrMem cart) state
            data

        | VRC1 state ->
            let data = VRC1.ppuRead addr (getChrMem cart) state
            data

        | J87 state ->
            let data = J87.ppuRead addr cart.chrRom state
            data

        | NROM _
        | _ ->
            let chr = getChrMem cart
            chr[addr]

    let ppuWrite addr value (vram : byte[]) cart =
        let addr = int addr

        match cart.mapper with
        | MMC3 state when cart.chrRam <> [||]->
            Mmc3.ppuWrite addr value cart.chrRam state

        | Namco163 state ->
            Namco163.ppuWrite addr value (getChrMem cart) vram state

        | _ when cart.chrRom <> [||] -> ()

        | _ ->
            cart.chrRam[int addr] <- value

        cart

    let ppuReadNameTable addr (vram : byte[]) cart =
        match cart.mapper with
        | Namco163 _ ->
            ppuRead (addr + 0x2000) vram cart
        | _ ->
            vram[addr]

    let ppuWriteNameTable addr value (vram : byte[]) cart =
        match cart.mapper with
        | Namco163 _ ->
            ppuWrite (addr + 0x2000) value vram cart
        | _ ->
            vram[addr] <- value
            cart


    let onPpuFetch addr mapper =
        match mapper with
        | MMC3 state -> Mmc3.onPpuFetch addr state
        | _ -> ()
    
    let scanlineCounter mapper =
        match mapper with
        | MMC3 state -> Mmc3.scanlineCounter state
        | _ -> ()

    let irqCounter mapper =
        match mapper with
        | Namco163 state -> Namco163.incrementIrqCounter state
        | _ -> ()

    let pollIrq mapper =
        match mapper with
        | MMC3 state ->
            Mmc3.pollIrq state
        | Namco163 state ->
            Namco163.pollIrq state
        | _ ->
            false
