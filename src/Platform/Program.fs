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

let frameToTexture (graphics: GraphicsDevice) (frame: Frame) : Texture2D =
    let tex = new Texture2D(graphics, Frame.Width, Frame.Height)
    let colorData =
        frame.data
        |> Array.map (fun (r, g, b) -> Color(int r, int g, int b))
    tex.SetData(colorData)
    tex

type basicNesGame(loadedRom) as this =

  inherit Game()
  let scale = 4
  let graphics = new GraphicsDeviceManager(this)
  let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
  let mutable texture = Unchecked.defaultof<Texture2D>

  let raw = loadedRom
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

  else
    // https://www.nesworld.com/article.php?system=nes&data=neshomebrew
    use game = new basicNesGame (loadRom "Alter_Ego.nes")
    game.Run()
    0