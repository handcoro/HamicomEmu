module Rom

open FsToolkit.ErrorHandling

type Mirroring =
  | Horizontal
  | Vertical
  | FourScreen

type Rom = {
  PrgRom: byte array
  ChrRom: byte array
  Mapper: byte
  ScreenMirroring: Mirroring
}

let nesTag = [| 0x4Euy; 0x45uy; 0x53uy; 0x1Auy |] // NES<EOF> タグ
let prgRomPageSize = 16 * 1024 // 16KB
let chrRomPageSize = 8 * 1024 // 8KB

let validateTag (raw: byte[]) =
    if raw.Length >= 4 && raw[0..3] = nesTag then
        Ok ()
    else
        Error "File is not in iNES file format"

let validateVersion (raw: byte[]) =
    let inesVer = (raw[7] >>> 2) &&& 0b11uy
    if inesVer = 0uy then
        Ok ()
    else
        Error "NES2.0 format is not supported"

let parseRom (raw : byte array) = 
  result {
    do! validateTag raw
    do! validateVersion raw

    let mapperLow = raw[6] >>> 4
    let mapperHigh = raw[7] &&& 0b1111_0000uy
    let mapper = mapperLow ||| mapperHigh

    let fourScreen = (raw[6] &&& 0b1000uy) <> 0uy
    let verticalMirroring = (raw[6] &&& 0b1uy) <> 0uy
    let screenMirroring =
      match fourScreen, verticalMirroring with
      | true,  _     -> FourScreen
      | false, true  -> Vertical
      | false, false -> Horizontal
    
    let prgRomSize = int raw[4] * prgRomPageSize
    let chrRomSize = int raw[5] * chrRomPageSize

    let hasTrainer = raw[6] &&& 0b100uy <> 0uy
    let prgStart = 16 + if hasTrainer then 512 else 0
    let chrStart = prgStart + prgRomSize
    // ロムのレイアウト
    let prgRom = raw[prgStart .. prgStart + prgRomSize - 1]
    let chrRom = raw[chrStart .. chrStart + chrRomSize - 1]

    return {
      PrgRom = prgRom
      ChrRom = chrRom
      Mapper = mapper
      ScreenMirroring = screenMirroring
    }
  }