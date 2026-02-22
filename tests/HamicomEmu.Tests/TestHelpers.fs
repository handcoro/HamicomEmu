module TestHelpers

open HamicomEmu.Cartridge
open HamicomEmu.Mapper.Common
open HamicomEmu.Mapper

let private chrRomPageSize = 8 * 1024 // 8KB

let private normalizePrgRom (prg: byte array) = // PRG ROM を 32KB に正規化
    match prg.Length with
    | len when len < 0x8000 ->
        let padded = Array.create 0x8000 0uy
        Array.blit prg 0 padded 0 len
        padded
    | len when len = 0x8000 -> prg
    | len when len > 0x8000 -> prg[0..0x7FFF] // 切り詰める
    | _ -> failwith "Unexpected PRG ROM size"

let testCartridge program =
    let prgRom = normalizePrgRom program
    let chrRom = Array.create chrRomPageSize 2uy

    {
        prgRom = prgRom
        chrRom = chrRom
        chrRam = [||]
        mapper = Factory.create 3 prgRom chrRom None
        screenMirroring = Vertical
    }
