namespace HamicomEmu.Input

module Types =

    type JoypadState = {
        strobe: bool // パラレル入力とシリアル出力のラッチ、読み取り位置のリセット
        buttonIdx: int
        buttonStatus: byte
    }
