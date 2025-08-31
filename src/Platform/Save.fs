namespace HamicomEmu.Platform

module Save =
    open System
    open System.IO
    open HamicomEmu.EmulatorCore

    let createSaveFilename cartridgeFilePath =
        cartridgeFilePath + ".save"

    let saveRamFile path emu =
        let data = EmulatorCore.fetchRamData emu
        match data with
        | Some raw ->
            try
                Ok(
                    use writer = new BinaryWriter(File.Open(path, FileMode.Create))
                    writer.Write(raw)
                )
            with e ->
                Error $"Failed to write savefile '{path}': {e.Message}"
        | None -> Error "No need to Save."

    let loadRamFile path =
        try
            Ok(File.ReadAllBytes(path))
        with e ->
            Error $"Failed to read savefile '{path}': {e.Message}"

    let loadCartridgeFile path =
        try
            Ok(File.ReadAllBytes(path))
        with e ->
            Error $"Failed to load Cartridge: {e.Message}"
