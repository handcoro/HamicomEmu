namespace HamicomEmu.Mapper

module J87 =

    open HamicomEmu.Mapper.Common

    type State = { bankSelect: byte }

    let ppuRead addr (chr: byte[]) state =
        let bankSize = 8 * 1024
        let totalBanks = chr.Length / bankSize

        let offset = getOffset (int state.bankSelect % totalBanks) bankSize 0
        chr[addr + offset]

    let writePrgRam value =
        let hi = value &&& 0b01uy <<< 1
        let lo = value &&& 0b10uy >>> 1
        let v = hi ||| lo
        let newState = { bankSelect = v &&& 0b11uy } // 0-1 bits 使用
        // printfn "[MAPPER CPU WRITE] Bank Select: %A" newState.bankSelect
        newState
