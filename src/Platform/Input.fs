namespace HamicomEmu.Platform

module Input =
    open HamicomEmu.EmulatorCore
    open HamicomEmu.Input
    open Microsoft.Xna.Framework.Input
    open HamicomEmu.Common

    type InputSource =
        | Keyboard of KeyboardState
        | Gamepad of GamePadState

    let mutable prevFuncKeyboardState = Keyboard.GetState()

    let functionKeyMap = [
        Keys.R, EmulatorCore.reset
        Keys.P, (fun emu -> EmulatorCore.powerOn emu.bus.cartridge)
    ]

    let keyMap = [
        Keys.Right, Joypad.Button.right
        Keys.Left, Joypad.Button.left
        Keys.Down, Joypad.Button.down
        Keys.Up, Joypad.Button.up
        Keys.Enter, Joypad.Button.start
        Keys.Space, Joypad.Button.select
        Keys.S, Joypad.Button.b
        Keys.A, Joypad.Button.a
    ]

    let padMap = [
        Buttons.DPadRight, Joypad.Button.right
        Buttons.DPadLeft, Joypad.Button.left
        Buttons.DPadDown, Joypad.Button.down
        Buttons.DPadUp, Joypad.Button.up
        Buttons.Start, Joypad.Button.start
        Buttons.Back, Joypad.Button.select
        Buttons.A, Joypad.Button.b
        Buttons.B, Joypad.Button.a
    ]

    let isKeyPressed (ks: KeyboardState) key = ks.IsKeyDown key
    let wasKeyPressed (prev: KeyboardState) key = prev.IsKeyDown key
    let isPadPressed (gs: GamePadState) button = gs.IsButtonDown button

    let handleJoypadInput (input: InputSource) joy =
        match input with
        | Keyboard ks ->
            keyMap |> List.fold (fun acc (key, button) -> acc |> Joypad.setButtonPressed button (isKeyPressed ks key)) joy
        | Gamepad gs when gs.IsConnected ->
            padMap |> List.fold (fun acc (padButton, button) -> acc |> Joypad.setButtonPressed button (isPadPressed gs padButton)) joy
        | _ -> joy

    let handleFunctionKeyInput (input: InputSource) emu =
        match input with
        | Keyboard ks ->
            let emu' =
                functionKeyMap
                |> List.fold (fun acc (key, func) ->
                    if isKeyPressed ks key && not (wasKeyPressed prevFuncKeyboardState key) then
                        func acc
                    else acc
                ) emu
            prevFuncKeyboardState <- ks
            emu'
        | _ -> emu