namespace HamicomEmu.Mapper

module Types =
    // TODO: ミラーリングの型の配置場所を考える
    type Mirroring =
        | Horizontal
        | Vertical
        | FourScreen

    type Mapper =
        | NROM of NromState // マッパー 0
        | UxROM of UxromState // マッパー2
        | CNROM of CnromState
        | J87 of J87State
        | VRC1 of VRC1State

    and NromState = unit
    and BankSelectState = { bankSelect: byte }
    and UxromState = BankSelectState
    and CnromState = BankSelectState
    and J87State = BankSelectState

    and VRC1State = {
        prgSelect0: byte
        prgSelect1: byte
        prgSelect2: byte
        mirroring: Mirroring
        chrSelect0: byte
        chrSelect1: byte
    }
