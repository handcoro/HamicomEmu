namespace HamicomEmu.Mapper

module Gxrom =

    open HamicomEmu.Mapper.Common

    type State = {
        chrBankSelect: byte
        prgBankSelect: byte
        }

    let init = {
        chrBankSelect = 0uy
        prgBankSelect = 0uy
    }

    let cpuRead addr (prg: byte[]) state =
        let bankSize = 32 * 1024
        let totalBanks = prg.Length

        let offset = getOffset (int state.prgBankSelect % totalBanks) bankSize 0x8000
        prg[addr + offset]


    let ppuRead addr (chr: byte[]) state =
        let bankSize = 8 * 1024
        let totalBanks = chr.Length / bankSize

        let offset = getOffset (int state.chrBankSelect % totalBanks) bankSize 0
        chr[addr + offset]

    let cpuWrite addr value =
        // 7  bit  0
        // ---- ----
        // xxPP xxCC
        //   ||   ||
        //   ||   ++- Select 8 KB CHR ROM bank for PPU $0000-$1FFF
        //   ++------ Select 32 KB PRG ROM bank for CPU $8000-$FFFF
        let chrSelect = value &&& 0b0000_0011uy
        let prgSelect = value &&& 0b0011_0000uy >>> 4
        let newState = { chrBankSelect = chrSelect; prgBankSelect = prgSelect }
        newState
