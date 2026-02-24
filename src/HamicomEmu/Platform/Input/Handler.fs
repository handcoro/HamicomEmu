namespace HamicomEmu.Platform.Input

module Handler =

    open HamicomEmu.Input
    open Commands
    open Microsoft.Xna.Framework.Input

    /// キーが押されているかチェック
    let isKeyPressed (ks: KeyboardState) key = ks.IsKeyDown key

    /// 前フレームでキーが押されていたかチェック
    let wasKeyPressed (prev: KeyboardState) key = prev.IsKeyDown key

    /// パッドボタンが押されているかチェック
    let isPadPressed (gs: GamePadState) button = gs.IsButtonDown button

    /// ジョイパッド入力を処理（キーボードまたはゲームパッド）
    /// byte 型のボタンフラグを使用
    let handleJoypadInput (input: InputSource) number joy =
        match input with
        | Keyboard ks ->
            getKeyMap number
            |> List.fold (fun accJoy (key, button) -> 
                if isKeyPressed ks key then
                    accJoy |> Joypad.setButtonPressed button true
                else
                    accJoy |> Joypad.setButtonPressed button false
            ) joy

        | Gamepad gs when gs.IsConnected ->
            padMap
            |> List.fold (fun accJoy (padButton, button) -> 
                if isPadPressed gs padButton then
                    accJoy |> Joypad.setButtonPressed button true
                else
                    accJoy |> Joypad.setButtonPressed button false
            ) joy

        | _ -> joy

    /// ファンクションキー入力を処理（リセット、電源等）
    let handleFunctionKeyInput (input: InputSource) (prevState: KeyboardState) emu =
        match input with
        | Keyboard ks ->
            let emu' =
                functionKeyMap
                |> List.fold (fun acc (key, func) ->
                    if isKeyPressed ks key && not (wasKeyPressed prevState key) then
                        func acc
                    else
                        acc
                ) emu

            (ks, emu')

        | _ ->
            (prevState, emu)

    /// ジョイパッド状態を統合して返す
    let mergeJoypadStates (input: InputSource) (number: int) (baseJoy: Types.JoypadState) emu =
        let joy = Joypad.init |> handleJoypadInput input number
        match input with
        | Keyboard ks ->
            let isMicActive = isKeyPressed ks microphoneKey
            Joypad.mergeStates (joy |> Joypad.setMicrophone isMicActive) baseJoy
        | _ -> joy
