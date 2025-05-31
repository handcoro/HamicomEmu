// open Expecto
// open Tests
open Cpu
open Bus
open Cartridge
open Tests
open Screen
open Render
open Trace
open Joypad

open System
open System.IO
open FsToolkit.ErrorHandling
open Expecto

// MonoGame メインゲームクラス
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let color v =
  match v with
  | 0uy -> Color.Black
  | 1uy -> Color.White
  | 2uy | 9uy  -> Color.Gray
  | 3uy | 10uy -> Color.Red
  | 4uy | 11uy -> Color.Green
  | 5uy | 12uy -> Color.Blue
  | 6uy | 13uy -> Color.Magenta
  | 7uy | 15uy -> Color.Yellow
  | _ -> Color.Cyan

let handleUserInput (ks: KeyboardState) bus =
  let keyMap = [
    (Keys.W, 0x77uy); (Keys.Up,    0x77uy)
    (Keys.S, 0x73uy); (Keys.Down,  0x73uy)
    (Keys.A, 0x61uy); (Keys.Left,  0x61uy)
    (Keys.D, 0x64uy); (Keys.Right, 0x64uy)
  ]

  let input =
    keyMap
    |> List.tryFind (fun (k, _) -> ks.IsKeyDown k)
    |> Option.map snd

  match input with
  | Some v -> bus |> memWrite 0xFFus v
  | None -> bus

let handleJoypadInput (ks: KeyboardState) joy =
  let keyMap = [
    Keys.Right, JoyButton.Right
    Keys.Left,  JoyButton.Left
    Keys.Down,  JoyButton.Down
    Keys.Up,    JoyButton.Up
    Keys.Enter, JoyButton.Start
    Keys.Space, JoyButton.Select
    Keys.S,     JoyButton.B
    Keys.A,     JoyButton.A
  ]
  
  keyMap
  |> List.fold (fun accJoy (key, button) ->
      let isDown = ks.IsKeyDown key
      accJoy |> setButtonPressed button isDown
    ) joy

let loadRom path =
  try
    Ok (File.ReadAllBytes(path))
  with
  | e -> Error $"Failed to load ROM: {e.Message}"

type pseudoNESGame() as this =
  inherit Game()

  let graphics = new GraphicsDeviceManager(this)
  let mutable spriteBatch: SpriteBatch = null
  let mutable whiteTex: Texture2D = null
  let size = 32
  let width, height = size, size
  let tileSize = 10
  let screenBuffer = Array.create (width * height) 0uy


  let rnd = Random()

  let raw = loadRom "roms/snake.nes"
  let parsed = raw |> Result.bind parseRom
  let mutable cpu = initialCpu
  let mutable bus = Unchecked.defaultof<Bus>

  do
    match parsed with
    | Ok rom ->
      bus <- initialBus rom
      let cpu', bus' = (cpu, bus) ||> reset
      cpu <- cpu'
      bus <- bus'
    | Error e -> failwith $"Failed to parse ROM: {e}"

    graphics.PreferredBackBufferWidth <- width * 10
    graphics.PreferredBackBufferHeight <- height * 10

  override _.Initialize() =
    this.IsFixedTimeStep <- true
    this.TargetElapsedTime <- TimeSpan.FromMilliseconds(0.15) // フレームの更新間隔
    // 60fps で Update する制限を外す
    // this.IsFixedTimeStep <- false
    // graphics.SynchronizeWithVerticalRetrace <- false
    graphics.ApplyChanges()
    base.Initialize()

  override _.LoadContent() =
    spriteBatch <- new SpriteBatch(this.GraphicsDevice)
    whiteTex <- new Texture2D(this.GraphicsDevice, 1, 1)
    whiteTex.SetData[| Color.White |]
    base.LoadContent()

  override _.Update(gameTime) =
    if Keyboard.GetState().IsKeyDown(Keys.Escape) then this.Exit()

    // CPU 実行 + 入力処理 + ランダム入力
    // この実装では Update のたびに 1 ステップ実行
    bus <-
      bus
      |> handleUserInput (Keyboard.GetState())
      |> memWrite 0xFEus (byte (rnd.Next(1, 16)))

    let cpu', bus' = (cpu, bus) ||> step
    cpu <- cpu'
    bus <- bus'
    // CPU のフラグを確認して BRK したらゲームを終了
    if cpu.P |> hasFlag Flags.B then this.Exit()

    base.Update(gameTime)

  override _.Draw(gameTime) =

    this.GraphicsDevice.Clear(Color.Black)
    spriteBatch.Begin()

    // 仮の画面表示：ビデオRAMの0x0200〜を仮にピクセルデータとして表示
    for y in 0 .. height - 1 do
      for x in 0 .. width - 1 do
        let addr = 0x0200us + uint16 (y * width + x)
        let value, _ = memRead addr bus
        let col = color value
        let rect = Rectangle(x * 10, y * 10, 10, 10)
        spriteBatch.Draw(whiteTex, rect, col)

    spriteBatch.End()
    base.Draw(gameTime)

let frameToTexture (graphics: GraphicsDevice) (frame: Frame) : Texture2D =
    let tex = new Texture2D(graphics, Frame.Width, Frame.Height)
    let colorData =
        frame.data
        |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))
    tex.SetData(colorData)
    tex

type tileViewer() as this =
  inherit Game()
  let scale = 4
  let graphics = new GraphicsDeviceManager(this)
  let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
  let mutable texture = Unchecked.defaultof<Texture2D>

  let raw = loadRom "roms/Alter_Ego.nes" // https://www.nesworld.com/article.php?system=nes&data=neshomebrew
  let parsed = raw |> Result.bind parseRom
  let mutable cpu = initialCpu
  let mutable bus = Unchecked.defaultof<Bus>

  do
    match parsed with
    | Ok rom ->
      bus <- initialBus rom
      let cpu', bus' = (cpu, bus) ||> reset
      cpu <- cpu'
      bus <- bus'
    | Error e -> failwith $"Failed to parse ROM: {e}"

    graphics.PreferredBackBufferWidth <- Screen.Frame.Width * scale
    graphics.PreferredBackBufferHeight <- Screen.Frame.Height * scale
    graphics.ApplyChanges()
  
  override _.Initialize() =
    this.IsFixedTimeStep <- true
    this.TargetElapsedTime <- TimeSpan.FromMilliseconds(0.15) // フレームの更新間隔
    base.Initialize()

  override _.LoadContent() =
    spriteBatch <- new SpriteBatch(this.GraphicsDevice)
    let tileFrame = Screen.showTiles bus.ppu.chr 0 [0 .. 255]
    texture <- frameToTexture this.GraphicsDevice tileFrame
    base.LoadContent()
  override _.Draw(gameTime) =
    this.GraphicsDevice.Clear(Color.Black)
    spriteBatch.Begin(samplerState = SamplerState.PointClamp)
    let destRect = Rectangle(0, 0, Screen.Frame.Width * scale, Screen.Frame.Height * scale)
    spriteBatch.Draw(texture, destRect, Color.White)
    spriteBatch.End()
    base.Draw(gameTime)
  override _.Update(gameTime) =
    if Keyboard.GetState().IsKeyDown(Keys.Escape) then this.Exit()
    base.Update(gameTime)

let printNameTables ppu =
    for i in 0 .. 3 do
        printfn $"--- Name Table {i} ---"
        printfn "%s" (dumpNameTable ppu i)

type basicNesGame() as this =

  inherit Game()
  let scale = 4
  let graphics = new GraphicsDeviceManager(this)
  let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
  let mutable texture = Unchecked.defaultof<Texture2D>

  let raw = loadRom "roms/Alter_Ego.nes" // https://www.nesworld.com/article.php?system=nes&data=neshomebrew
  let parsed = raw |> Result.bind parseRom
  let mutable cpu = initialCpu
  let mutable bus = Unchecked.defaultof<Bus>
  
  let mutable frame = initialFrame

  do
    match parsed with
    | Ok rom ->
      bus <- initialBus rom
      let cpu', bus' = reset cpu bus
      cpu <- cpu'
      bus <- bus'
    | Error e -> failwith $"Failed to parse ROM: {e}"

    graphics.PreferredBackBufferWidth <- Screen.Frame.Width * scale
    graphics.PreferredBackBufferHeight <- Screen.Frame.Height * scale
    graphics.ApplyChanges()

  override _.Initialize() =
    this.IsFixedTimeStep <- true
    this.TargetElapsedTime <- TimeSpan.FromMilliseconds(1) // フレームの更新間隔
    base.Initialize()

  override _.LoadContent() =
    spriteBatch <- new SpriteBatch(this.GraphicsDevice)
    texture <- new Texture2D(this.GraphicsDevice, Frame.Width, Frame.Height)
    base.LoadContent()

  override _.Draw(gameTime) =
    this.GraphicsDevice.Clear(Color.Black)
    spriteBatch.Begin(samplerState = SamplerState.PointClamp)
    frame <- render bus.ppu frame
    let colorData =
      frame.data |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))
    texture.SetData(colorData)
    let destRect = Rectangle(0, 0, Screen.Frame.Width * scale, Screen.Frame.Height * scale)
    spriteBatch.Draw(texture, destRect, Color.White)
    spriteBatch.End()
    base.Draw(gameTime)

  override _.Update(gameTime) =
    if Keyboard.GetState().IsKeyDown(Keys.Escape) then this.Exit()
    // printfn "%s" (trace cpu bus)
    let joy = bus.joy1 |> handleJoypadInput (Keyboard.GetState())
    bus <- {bus with joy1 = joy }

    for _ in 0 .. 600 do // 時間がかかるので 1 フレームで一気に命令処理してしまう
      let cpu', bus' = (cpu, bus) ||> step
      cpu <- cpu'
      bus <- bus'

    base.Update(gameTime)

[<EntryPoint>]
let main argv =
  if argv |> Array.exists ((=) "--test") then
    // --test を除いた引数だけ Expecto に渡す
    let filteredArgs = argv |> Array.filter (fun a -> a <> "--test")
    runTestsWithArgs defaultConfig filteredArgs Tests.tests

  elif argv |> Array.exists ((=) "--showtiles") then
    // タイル表示ロジック
    use game = new tileViewer()
    game.Run()
    0

  elif argv |> Array.exists ((=) "--snake") then
    // スネークゲーム
    use game = new pseudoNESGame()
    game.Run()
    0
  else
    use game = new basicNesGame()
    game.Run()
    0