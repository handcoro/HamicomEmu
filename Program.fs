// open Expecto
// open Tests
open Cpu
open Bus
open Cartridge
open Tests

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
    | Ok rom -> bus <- initialBus rom
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
        let value = addr |> memRead bus
        let col = color value
        let rect = Rectangle(x * 10, y * 10, 10, 10)
        spriteBatch.Draw(whiteTex, rect, col)

    spriteBatch.End()
    base.Draw(gameTime)

[<EntryPoint>]
let main argv =
  if argv |> Array.exists ((=) "--test") then
    // --test を除いた引数だけ Expecto に渡す
    let filteredArgs = argv |> Array.filter (fun a -> a <> "--test")
    runTestsWithArgs defaultConfig filteredArgs Tests.tests
  else
    use game = new pseudoNESGame()
    game.Run()
    0
