namespace HamicomEmu.Mapper

module Types =

    type Mapper =
        | NROM of NromState // マッパー 0
        | MMC1 of MMC1.State // マッパー 1
        | UxROM of Uxrom.State // マッパー2
        | CNROM of CnromState
        | MMC3 of Mmc3.State
        | GxROM of Gxrom.State
        | J87 of J87.State
        | VRC1 of VRC1.State

    and NromState = unit
    and BankSelectState = { bankSelect: byte }
    and CnromState = BankSelectState
