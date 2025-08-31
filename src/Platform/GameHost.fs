namespace HamicomEmu.Platform

module GameHost =
    open System
    open System.IO
    open HamicomEmu.EmulatorCore
    open HamicomEmu.Common
    open HamicomEmu.Cartridge
    open HamicomEmu.Ppu.Screen
    open HamicomEmu.Ppu.Renderer
    open HamicomEmu.Apu
    open HamicomEmu.Input
    open HamicomEmu.Trace
    open HamicomEmu.Platform.AudioEngine
    open HamicomEmu.Platform.Input
    open HamicomEmu.Platform.Save

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input

    let defaultCart = "roms/Alter_Ego.nes"

    type basicNesGame(raw, cartridgePath, traceFn) as this =
        inherit Game()
        let scale = 4
        let sampleRate = 44100
        let graphics = new GraphicsDeviceManager(this)
        let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
        let mutable texture = Unchecked.defaultof<Texture2D>
        let mutable audioEngine = AudioEngine(sampleRate)

        let parsed = parseCartridge raw

        let cartPath = cartridgePath
        let mutable emu = Unchecked.defaultof<EmulatorCore.EmulatorState>
        let traceFn = traceFn

        do
            match parsed with
            | Ok cart ->
                emu <- EmulatorCore.powerOn cart
                let emu' =
                    let saveFile = createSaveFilename cartPath
                    match loadRamFile saveFile with
                    | Ok data ->
                        printfn $"Savedata imported successfully."
                        EmulatorCore.storeRamData data emu
                    | Error _ ->
                        emu

                emu <- emu'
            | Error e -> failwith $"Failed to parse ROM: {e}"

            graphics.PreferredBackBufferWidth <- width * scale
            graphics.PreferredBackBufferHeight <- height * scale
            graphics.ApplyChanges()

        override _.Dispose(disposing: bool) =
            let savePath = createSaveFilename cartPath
            match saveRamFile savePath emu with
            | Error e ->
                printfn $"{e}"
            | _ ->
                printfn $"Savedata exported successfully."

            if disposing then
                audioEngine.Dispose()
            base.Dispose(disposing)

        override _.Initialize() =
            this.IsFixedTimeStep <- true
            this.TargetElapsedTime <- TimeSpan.FromSeconds(1.0 / 60.0)
            base.Initialize()

        override _.LoadContent() =
            spriteBatch <- new SpriteBatch(this.GraphicsDevice)
            texture <- new Texture2D(this.GraphicsDevice, width, height)
            // GUI を後でここで初期化する（Gui.init this など）
            base.LoadContent()

        override _.Draw(gameTime) =
            this.GraphicsDevice.Clear(Color.Black)
            spriteBatch.Begin(samplerState = SamplerState.PointClamp)
            let fr = renderFrame emu.bus.ppu

            let colorData =
                fr |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))

            texture.SetData(colorData)
            let destRect = Rectangle(0, 0, width * scale, height * scale)
            spriteBatch.Draw(texture, destRect, Color.White)
            spriteBatch.End()
            base.Draw(gameTime)

        override _.Update(gameTime) =
            let ks = Keyboard.GetState()
            let gs = GamePad.GetState(PlayerIndex.One)

            if ks.IsKeyDown(Keys.Escape) then
                this.Exit()

            emu <- handleFunctionKeyInput (InputSource.Keyboard ks) emu

            let keyJoy = Joypad.init |> handleJoypadInput (InputSource.Keyboard ks)
            let padJoy = Joypad.init |> handleJoypadInput (InputSource.Gamepad gs)
            let joy = Joypad.mergeStates keyJoy padJoy

            emu <- { emu with bus.joy1.buttonStatus = joy.buttonStatus }

            let cpuClock = Constants.cpuClockNTSC
            let cyclesPerSample = cpuClock / float sampleRate
            let samplesPerFrame = sampleRate / 60

            let mutable cycleAcc = 0.0
            let samples = ResizeArray<float32>()

            for _ in 1..samplesPerFrame do
                cycleAcc <- cycleAcc + cyclesPerSample
                while cycleAcc >= 1.0 do
                    let emu', used = EmulatorCore.tick emu traceFn
                    emu <- emu'
                    cycleAcc <- cycleAcc - float used

                let sample = Apu.mix emu.bus.apu
                samples.Add(sample)

            audioEngine.Submit(samples |> Seq.toArray)

            base.Update(gameTime)

    let run path traceFn =
        match loadCartridgeFile path with
        | Ok raw ->
            use game = new basicNesGame (raw, path, traceFn)
            game.Run()
        | Error msg ->
            printfn "%s" msg
            // 予備としてデフォルトを試す
            match loadCartridgeFile defaultCart with
            | Ok raw ->
                use game = new basicNesGame (raw, defaultCart, traceFn)
                game.Run()
            | Error msg2 ->
                printfn "%s" msg2