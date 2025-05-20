open Expecto
open Tests

open SnakeGame
open Cpu

open System

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

let handleUserInput (ks: KeyboardState) cpu =
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
    | Some v -> memWrite 0xFFus v cpu
    | None -> cpu

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


    let mutable cpu =
        initialCpu
        |> load gameCode
        |> reset

    do
        graphics.PreferredBackBufferWidth <- width * 10
        graphics.PreferredBackBufferHeight <- height * 10

    let readScreenState cpu (frame: byte[]) = // バッファを持って変更を検出するけどまだ使ってない
        let mutable updated = false

        for i = 0x0200 to 0x05FF do
            let colorIdx = memRead cpu (uint16 i)
            if frame[i - 0x200] <> colorIdx then
                frame[i - 0x200] <- colorIdx
                updated <- true
        updated

    let drawScreen (spriteBatch: SpriteBatch) (whiteTex: Texture2D) (frame: byte[]) = // 同上
        spriteBatch.Begin()
        let mutable idx = 0
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let idx = y * width + x
                let colorIdx = frame[idx]
                let c = color colorIdx
                let rect = Rectangle(x * tileSize, y * tileSize, tileSize, tileSize)
                spriteBatch.Draw(whiteTex, rect, c)
        spriteBatch.End()

    override _.Initialize() =
        this.IsFixedTimeStep <- true
        this.TargetElapsedTime <- TimeSpan.FromMilliseconds(0.15)
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

        // CPU 実行＋入力処理＋ランダム入力
        cpu <-
            cpu
            |> handleUserInput (Keyboard.GetState())
            |> memWrite 0xFEus (byte (rnd.Next(1, 16)))
            |> step
        if cpu.P |> hasFlag Flags.B then this.Exit()

        base.Update(gameTime)

    override _.Draw(gameTime) =

        this.GraphicsDevice.Clear(Color.Black)
        spriteBatch.Begin()

        // 仮の画面表示：ビデオRAMの0x0200〜を仮にピクセルデータとして表示
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let addr = 0x0200us + uint16 (y * width + x)
                let value = addr |> memRead cpu
                let col = color value
                let rect = Rectangle(x * 10, y * 10, 10, 10)
                spriteBatch.Draw(whiteTex, rect, col)

        spriteBatch.End()
        base.Draw(gameTime)

[<EntryPoint>]
let main argv =
    use game = new pseudoNESGame()
    game.Run()
    0

// [<EntryPoint>]
// let main argv =
//     runTestsWithArgs defaultConfig argv tests