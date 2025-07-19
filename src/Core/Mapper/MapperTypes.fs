namespace HamicomEmu.Mapper

module Types =

    type Mapper =
        | NROM of NromState // マッパー 0
        | MMC1 of MMC1.State // マッパー 1
        | UxROM of UxromState // マッパー2
        | CNROM of CnromState
        | J87 of J87State
        | VRC1 of VRC1.State

    and NromState = unit
    and BankSelectState = { bankSelect: byte }
    and UxromState = BankSelectState
    and CnromState = BankSelectState
    and J87State = BankSelectState
