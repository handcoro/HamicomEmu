namespace HamicomEmu.Mapper.Types

type Mapper =
| NROM of NromState // マッパー 0
| UxROM of UnromState // マッパー2

and NromState = unit
and UnromState = { bankSelect: byte }
