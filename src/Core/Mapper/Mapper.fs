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
        | J87 _ ->
            let newState = J87.writePrgRam value
            J87 newState, ()
        | _ ->
            printfn "[MAPPER %A] write to PRG Ram is not supported." cart.mapper
            cart.mapper, ()

    let getPrgRam cart =
        match cart.mapper with
        | MMC1 state ->
            Some state.prgRam
        | _ ->
            None

    let setPrgRam data cart =
        match cart.mapper with
        | MMC1 state ->
            MMC1.setPrgRam data state
            cart
        | _ ->
            cart

    let cpuRead (addr: uint16) cart =
        let addr = int addr
        let prg = cart.prgRom

        match cart.mapper with
        | MMC1 state ->
            MMC1.cpuRead addr prg state

        | UxROM state -> Uxrom.cpuRead addr prg state

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

    let getChrMem cart =
        if cart.chrRom <> [||] then
            cart.chrRom
        else
            cart.chrRam

    let ppuRead addr cart =
        let addr = int addr

        match cart.mapper with
        | MMC1 state ->
            let data = MMC1.ppuRead addr (getChrMem cart) state
            data

        | CNROM state ->
            let addr' = getChrAddressCnrom addr cart state
            cart.chrRom[addr']

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

    let ppuReadRange startAddr endAddr cart mapper =
        match cart.mapper with
        | MMC1 _
        | CNROM _
        | VRC1 _
        | J87 _ -> Array.init (endAddr - startAddr + 1) (fun i -> ppuRead (startAddr + i) cart)

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
