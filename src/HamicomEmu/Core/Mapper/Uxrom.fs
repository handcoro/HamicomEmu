namespace HamicomEmu.Mapper

module Uxrom =

    open HamicomEmu.Mapper.Common

    type State = { bankSelect: byte }

    let cpuRead addr (prg : byte[]) state =
        let bankSize = 16 * 1024 // 16 KB
        let totalBanks = prg.Length / bankSize

        match addr &&& 0xC000 with
        | 0x8000 -> // 可変バンク
            let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0x8000
            prg[addr + offset]

        | 0xC000 ->
            // 固定バンク（最後尾）
            let offset = getOffset (totalBanks - 1) bankSize 0xC000
            prg[addr + offset]

        | _ ->
            // PRG-ROM の外参照
            0uy
    let cpuWrite addr value =
        { bankSelect = value &&& 0x0Fuy }
