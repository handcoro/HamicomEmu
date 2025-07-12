open HamicomEmu.EmulatorCore
open HamicomEmu.Common
open HamicomEmu.Cartridge
open Tests
open HamicomEmu.Ppu.Screen
open HamicomEmu.Ppu.Renderer
open HamicomEmu.Apu
open HamicomEmu.Trace
open HamicomEmu.Platform.AudioEngine
open Joypad

open System
open System.IO
open FsToolkit.ErrorHandling
open Expecto

// MonoGame メインゲームクラス
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type InputSource =
    | Keyboard of KeyboardState
    | Gamepad of GamePadState

let keyMap =
    [ Keys.Right, JoyButton.Right
      Keys.Left, JoyButton.Left
      Keys.Down, JoyButton.Down
      Keys.Up, JoyButton.Up
      Keys.Enter, JoyButton.Start
      Keys.Space, JoyButton.Select
      Keys.S, JoyButton.B
      Keys.A, JoyButton.A ]

/// XBox Gamepad 前提の設定
let padMap =
    [ Buttons.DPadRight, JoyButton.Right
      Buttons.DPadLeft, JoyButton.Left
      Buttons.DPadDown, JoyButton.Down
      Buttons.DPadUp, JoyButton.Up
      Buttons.Start, JoyButton.Start
      Buttons.Back, JoyButton.Select
      Buttons.A, JoyButton.B
      Buttons.B, JoyButton.A ]

let isKeyPressed (ks: KeyboardState) key = ks.IsKeyDown key

let isPadPressed (gs: GamePadState) button = gs.IsButtonDown button

let handleJoypadInput (input: InputSource) (joy: JoypadState) =
    let joy =
        match input with
        | Keyboard ks ->
            keyMap
            |> List.fold (fun accJoy (key, button) -> accJoy |> setButtonPressed button (isKeyPressed ks key)) joy
        | Gamepad gs when gs.IsConnected ->
            padMap
            |> List.fold
                (fun accJoy (padButton, button) -> accJoy |> setButtonPressed button (isPadPressed gs padButton))
                joy
        | _ -> joy

    joy

let loadRom path =
    try
        Ok(File.ReadAllBytes(path))
    with e ->
        Error $"Failed to load ROM: {e.Message}"

let frameToTexture (graphics: GraphicsDevice) (frame: Frame) : Texture2D =
    let tex = new Texture2D(graphics, Frame.width, Frame.height)

    let colorData =
        frame.data |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))

    tex.SetData(colorData)
    tex

type basicNesGame(loadedRom, traceFn) as this =

    inherit Game()
    let scale = 4
    let sampleRate = 44100
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>
    let mutable audioEngine = AudioEngine(sampleRate)

    let raw = loadedRom
    let parsed = raw |> Result.bind parseCartridge
    let mutable emu = Unchecked.defaultof<EmulatorCore.EmulatorState>
    let mutable frame = initialFrame
    let traceFn = traceFn

    do
        match parsed with
        | Ok rom ->
            emu <- EmulatorCore.initial rom
            let emu' = EmulatorCore.reset emu
            emu <- emu'
        | Error e -> failwith $"Failed to parse ROM: {e}"

        graphics.PreferredBackBufferWidth <- Frame.width * scale
        graphics.PreferredBackBufferHeight <- Frame.height * scale
        graphics.ApplyChanges()

    override _.Initialize() =
        this.IsFixedTimeStep <- true
        this.TargetElapsedTime <- TimeSpan.FromSeconds(1.0 / 60.0) // フレームの更新間隔
        base.Initialize()

    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        texture <- new Texture2D(this.GraphicsDevice, Frame.width, Frame.height)
        base.LoadContent()

    override _.Draw(gameTime) =
        this.GraphicsDevice.Clear(Color.Black)
        spriteBatch.Begin(samplerState = SamplerState.PointClamp)
        frame <- renderScanlineBased emu.bus.ppu emu.ppuSnapshot frame

        let colorData =
            frame.data |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))

        texture.SetData(colorData)
        let destRect = Rectangle(0, 0, Frame.width * scale, Frame.height * scale)
        spriteBatch.Draw(texture, destRect, Color.White)
        spriteBatch.End()
        base.Draw(gameTime)

    override _.Update(gameTime) =
        let ks = Keyboard.GetState()
        let gs = GamePad.GetState(PlayerIndex.One)

        if ks.IsKeyDown(Keys.Escape) then
            this.Exit()

        let keyJoy = initialJoypad |> handleJoypadInput (Keyboard ks)
        let padJoy = initialJoypad |> handleJoypadInput (Gamepad gs)
        let joy = mergeJoypadStates keyJoy padJoy

        emu <-
            { emu with
                bus.joy1.buttonStatus = joy.buttonStatus }

        let sampleRate = 44100
        let cpuClock = Constants.cpuClockNTSC
        let cyclesPerSample = cpuClock / float sampleRate // ≒ 40.584
        let samplesPerFrame = sampleRate / 60 // = 735

        // 1フレーム分のサンプルを作るバッファ
        let samples = ResizeArray<float32>()

        for _ in 1..samplesPerFrame do
            // 1フレームに達するまで CPU を進める
            let mutable cyclesRemain = cyclesPerSample

            while cyclesRemain > 0.0 do
                let emu', used = EmulatorCore.tick emu traceFn // ← step が消費サイクル数を返すように
                emu <- emu'
                cyclesRemain <- cyclesRemain - float used

            // APU から 1 サンプル取り出す
            let sample, apu' = Apu.mix (1.0 / float sampleRate) emu.bus.apu
            emu <- { emu with bus.apu = apu' }

            samples.Add(sample)

        // AudioEngine へ一括送信
        audioEngine.Submit(samples |> Seq.toList)


        base.Update(gameTime)

/// https://www.nesworld.com/article.php?system=nes&data=neshomebrew
let defaultRom = "roms/Alter_Ego.nes"

[<EntryPoint>]
let main argv =
    if argv |> Array.exists ((=) "--test") then
        // --test を除いた引数だけ Expecto に渡す
        let filteredArgs = argv |> Array.filter (fun a -> a <> "--test")
        runTestsWithArgs defaultConfig filteredArgs Tests.tests
    else
        let traceFn =
            if argv |> Array.exists ((=) "--trace") then
                traceAndPrint
            else
                fun _ -> ()

        match argv |> Array.tryHead with
        | Some romPath ->
            match loadRom romPath with
            | Ok rom ->
                use game = new basicNesGame (loadRom romPath, traceFn)
                game.Run()
                0
            | Error msg ->
                use game = new basicNesGame (loadRom defaultRom, traceFn)
                game.Run()
                0
        | None ->
            use game = new basicNesGame (loadRom defaultRom, traceFn)
            game.Run()
            0
