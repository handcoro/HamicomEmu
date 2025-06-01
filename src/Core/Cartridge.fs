module Cartridge

open FsToolkit.ErrorHandling

type Mirroring =
  | Horizontal
  | Vertical
  | FourScreen

type Rom = {
  prgRom: byte array
  chrRom: byte array
  mapper: byte
  screenMirroring: Mirroring
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
    // NES2.0 は互換性があるのでバージョンチェックしなくても大丈夫
    // do! validateVersion raw

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
      prgRom = prgRom
      chrRom = chrRom
      mapper = mapper
      screenMirroring = screenMirroring
    }
  }

type TestRom = {
  header: byte array
  trainer: byte array option
  prgRom: byte array
  chrRom: byte array
}
let createRom rom =
  Array.concat [
    rom.header
    match rom.trainer with
    | Some t -> t
    | None -> [||]
    rom.prgRom
    rom.chrRom
  ]

let normalizePrgRom (prg : byte array) = // PRG ROM を 32KB に正規化
  match prg.Length with
  | len when len < 0x8000 ->
    let padded = Array.create 0x8000 0uy
    Array.blit prg 0 padded 0 len
    padded
  | len when len = 0x8000 -> prg
  | len when len > 0x8000 -> prg[0..0x7FFF] // 切り詰める
  | _ -> failwith "Unexpected PRG ROM size"

let testRom program =
  let prgRomContents = normalizePrgRom program
  let rom = {
    header = Array.append nesTag [| 0x02uy; 0x01uy; 0x31uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy |]
    trainer = None
    prgRom = prgRomContents
    chrRom = Array.create chrRomPageSize 2uy
  }
  rom |> createRom |> parseRom |> Result.defaultWith (fun msg -> failwith $"Cannot create test rom: %s{msg}")