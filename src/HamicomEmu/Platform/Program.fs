module HamicomEmu.Main

open HamicomEmu.EmulatorCore
open HamicomEmu.Common
open HamicomEmu.Cartridge
open HamicomEmu.Ppu.Screen
open HamicomEmu.Ppu.Renderer
open HamicomEmu.Apu
open HamicomEmu.Trace
open HamicomEmu.Platform.Audio.Engine
open HamicomEmu.Platform.Audio
open HamicomEmu.Platform.Input
open HamicomEmu.Platform.Input.Commands
open HamicomEmu.Input

open System
open System.IO
open FsToolkit.ErrorHandling

// MonoGame メインゲームクラス
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

// 入力処理は Platform/Input/{Commands,Handler}.fs にて管理

// 電源やリセット操作用の直前のキー状態を覚えておく
let mutable prevFuncKeyboardState = Keyboard.GetState()

let handleFunctionKeyInput (input: InputSource) emu =
    match input with
    | Keyboard ks ->
        let emu' =
            functionKeyMap
            |> List.fold (fun acc (key, func) ->
                if ks.IsKeyDown(key) && not (prevFuncKeyboardState.IsKeyDown(key)) then
                    func acc
                else
                    acc
            ) emu

        prevFuncKeyboardState <- ks
        emu'
    | _ ->
        emu

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
    
#if DEBUG
    let oversampleFactor = 2
#else
    let oversampleFactor = 4
#endif
    let mutable audioProcessor = Processor.create sampleRate oversampleFactor

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

    /// リソース開放
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
        this.TargetElapsedTime <- TimeSpan.FromSeconds(1.0 / 60.0) // フレームの更新間隔
        base.Initialize()

    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        texture <- new Texture2D(this.GraphicsDevice, width, height)
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
        
        // リセット機能など
        emu <- handleFunctionKeyInput (Commands.Keyboard ks) emu

        // マイク入力
        let isMicActive = Handler.isKeyPressed ks Commands.microphoneKey
        let joy1 =
            Joypad.mergeStates
                (
                    Joypad.init
                    |> Handler.handleJoypadInput (Commands.Keyboard ks) 1
                    |> Joypad.setMicrophone isMicActive
                )
                (Joypad.init |> Handler.handleJoypadInput (Commands.Gamepad gs) 1)

        let joy2 = Joypad.init |> Handler.handleJoypadInput (Commands.Keyboard ks) 2

        emu <- {
            emu with
                bus.joy1.buttonStatus = joy1.buttonStatus
                bus.joy2.buttonStatus = joy2.buttonStatus
                bus.joy1.microphone = joy1.microphone
        }

        // Audio処理: 1フレーム分のサンプルを生成（オーバーサンプリング適用）
        let deltaSeconds = gameTime.ElapsedGameTime.TotalSeconds
        let desiredSamples = int (deltaSeconds * float sampleRate)
        let audioProcessor', emu', samples = Processor.generateFrame audioProcessor emu traceFn desiredSamples
        audioProcessor <- audioProcessor'
        emu <- emu'
        
        // AudioEngine へ一括送信
        audioEngine.Submit(samples)


        base.Update(gameTime)

/// https://www.nesworld.com/article.php?system=nes&data=neshomebrew
let defaultCart = "roms/Alter_Ego.nes"

let loadDefaultCartridge f =
    printfn $"Load default Cartridge {defaultCart}"
    match loadCartridgeFile defaultCart with
    | Ok raw ->
        use game = new basicNesGame (raw, defaultCart, f)
        game.Run()
    | Error msg ->
        printfn $"{msg}"

let traceFn argv =
    if argv |> Array.exists ((=) "--trace") then
        traceAndPrint
    else
        fun _ -> ()

let processCartridge path traceFn =
    match loadCartridgeFile path with
    | Ok raw ->
        use game = new basicNesGame (raw, path, traceFn)
        game.Run()
        0
    | Error msg ->
        printfn $"{msg}"
        loadDefaultCartridge traceFn
        0

[<EntryPoint>]
let main argv =
    let trace = traceFn argv

    match argv |> Array.tryHead with
    | Some path ->
        processCartridge path trace
    | None ->
        loadDefaultCartridge trace
        0
