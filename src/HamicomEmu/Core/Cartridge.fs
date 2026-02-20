namespace HamicomEmu

module Cartridge =
    open FsToolkit.ErrorHandling
    open HamicomEmu.Mapper.Types
    open HamicomEmu.Mapper.Common
    open HamicomEmu.Mapper

    type Cartridge = {
        prgRom: byte array
        chrRom: byte array
        chrRam: byte array
        mapper: Mapper
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

    let parseCartridge (raw: byte array) =
        result {
            do! validateTag raw
            // NES2.0 は互換性があるのでバージョンチェックしなくても大丈夫
            // do! validateVersion raw
            let mapperLow = raw[6] >>> 4
            let mapperHigh = raw[7] &&& 0b1111_0000uy
            let mapper = mapperLow ||| mapperHigh |> int
            let fourScreen = (raw[6] &&& 0b1000uy) <> 0uy
            let verticalMirroring = (raw[6] &&& 0b1uy) <> 0uy

            let screenMirroring =
                match fourScreen, verticalMirroring with
                | true, _ -> FourScreen
                | false, true -> Vertical
                | false, false -> Horizontal

            let prgRomSize = int raw[4] * prgRomPageSize
            let chrRomSize = int raw[5] * chrRomPageSize
            let hasTrainer = raw[6] &&& 0b100uy <> 0uy
            let prgStart = 16 + if hasTrainer then 512 else 0
            let chrStart = prgStart + prgRomSize
            // ロムのレイアウト
            let prgRom = Array.sub raw prgStart prgRomSize
            let chrRom = Array.sub raw chrStart chrRomSize
            // とりあえず CHR RAM は 8 KB とする
            let chrRam = if chrRomSize = 0 then Array.zeroCreate 0x2000 else [||]

            return {
                prgRom = prgRom
                chrRom = chrRom
                chrRam = chrRam
                mapper = Factory.create mapper prgRom (if chrRom <> [||] then chrRom else chrRam)
                screenMirroring = screenMirroring
            }
        }