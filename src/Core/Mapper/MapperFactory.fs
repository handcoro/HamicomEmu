namespace HamicomEmu.Mapper

module Factory =

    open HamicomEmu.Mapper.Types

    /// ID から Mapper を生成
    let create n =
        match n with
        | 0 -> NROM ()
        | 1 -> MMC1 MMC1.init
        | 2 -> UxROM { bankSelect = 0uy }
        | 3 -> CNROM { bankSelect = 0uy }
        | 66 -> GxROM Gxrom.init
        | 75 -> VRC1 VRC1.init
        | 87 -> J87 { bankSelect = 0uy }
        | _ ->
            printfn "Unsupported mapper: %A" n
            NROM ()
