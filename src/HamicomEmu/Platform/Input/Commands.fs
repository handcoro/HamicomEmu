namespace HamicomEmu.Platform.Input

module Commands =

    open Microsoft.Xna.Framework.Input
    open HamicomEmu.EmulatorCore
    open HamicomEmu.Input

    type InputSource =
        | Keyboard of KeyboardState
        | Gamepad of GamePadState

    /// ゲーム外操作用のキーバインディング
    let functionKeyMap: (Keys * (EmulatorCore.EmulatorState -> EmulatorCore.EmulatorState)) list = [
        Keys.R, EmulatorCore.reset
        Keys.P, (fun emu -> EmulatorCore.powerOn emu.bus.cartridge)
    ]

    /// 1Pコントローラー のキーバインディング（方向キー + EnterSpaceで操作）
    let keyMap: (Keys * byte) list = [
        Keys.Right, Joypad.Button.right
        Keys.Left, Joypad.Button.left
        Keys.Down, Joypad.Button.down
        Keys.Up, Joypad.Button.up
        Keys.Enter, Joypad.Button.start
        Keys.Space, Joypad.Button.select
        Keys.S, Joypad.Button.b
        Keys.A, Joypad.Button.a
    ]

    /// 2Pコントローラー のキーバインディング（HJKL配置）
    let keyMap2: (Keys * byte) list = [
        Keys.J, Joypad.Button.right
        Keys.G, Joypad.Button.left
        Keys.H, Joypad.Button.down
        Keys.Y, Joypad.Button.up
        Keys.K, Joypad.Button.b
        Keys.L, Joypad.Button.a
    ]

    /// XBox Gamepad のキーバインディング
    let padMap: (Buttons * byte) list = [
        Buttons.DPadRight, Joypad.Button.right
        Buttons.DPadLeft, Joypad.Button.left
        Buttons.DPadDown, Joypad.Button.down
        Buttons.DPadUp, Joypad.Button.up
        Buttons.Start, Joypad.Button.start
        Buttons.Back, Joypad.Button.select
        Buttons.A, Joypad.Button.b
        Buttons.B, Joypad.Button.a
    ]

    /// マイク入力キー
    let microphoneKey = Keys.M

    let getKeyMap number =
        match number with
        | 1 -> keyMap
        | 2 -> keyMap2
        | _ -> keyMap
