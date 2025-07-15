namespace HamicomEmu.Mapper

module Types =
    // TODO: ミラーリングの型の配置場所を考える
    type Mirroring =
        | Horizontal
        | Vertical
        | FourScreen

    type Mapper =
        | NROM of NromState // マッパー 0
        | MMC1 of Mmc1State
        | UxROM of UxromState // マッパー2
        | CNROM of CnromState
        | J87 of J87State
        | VRC1 of VRC1State

    and NromState = unit
    and Mmc1State = {
        shiftReg: byte
        shiftCount: int
        control: byte
        chrBank0: byte
        chrBank1: byte
        prgBank: byte
    }
    and BankSelectState = { bankSelect: byte }
    and UxromState = BankSelectState
    and CnromState = BankSelectState
    and J87State = BankSelectState

    and VRC1State = {
        prgBank0: byte
        prgBank1: byte
        prgBank2: byte
        mirroring: Mirroring
        chrBank0: byte
        chrBank1: byte
    }
