namespace HamicomEmu.Mapper

module Types =

  type Mapper =
  | NROM of NromState // マッパー 0
  | UxROM of UxromState // マッパー2
  | J87 of J87State

  and NromState = unit
  and BankSelectState = { bankSelect: byte }
  and UxromState = BankSelectState
  and J87State = BankSelectState
