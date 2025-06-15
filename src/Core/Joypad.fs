module Joypad

module JoyButton =
  let Right  = 0b1000_0000uy
  let Left   = 0b0100_0000uy
  let Down   = 0b0010_0000uy
  let Up     = 0b0001_0000uy
  let Start  = 0b0000_1000uy
  let Select = 0b0000_0100uy
  let B      = 0b0000_0010uy
  let A      = 0b0000_0001uy

type Joypad = {
  strobe: bool // パラレル入力とシリアル出力のラッチ、読み取り位置のリセット
  buttonIdx: int
  buttonStatus: byte
}

let initialJoypad = {
  strobe = false
  buttonIdx = 0
  buttonStatus = 0uy
}

/// strobe の書き込み
/// TODO: 拡張ポート対応
let writeJoypad value joy =
  let strobe = if value &&& 1uy <> 0uy then true else false
  let idx = if strobe then 0 else joy.buttonIdx // strobe がセットされていれば読み取りは A ボタン固定
  { joy with strobe = strobe; buttonIdx = idx }

/// 1 ビットずつ入力状態を返す
let readJoypad joy =
  let idx = joy.buttonIdx
  if idx > 7 then
    1uy, joy // 8 回読んだら以降は常に 1 を返す仕様
  else
    let bit = (joy.buttonStatus >>> idx) &&& 1uy
    let idx' = if not joy.strobe && idx <= 7 then idx + 1 else idx
    bit, { joy with buttonIdx = idx'}

/// 入力状態の更新
/// 本来は strobe をセットした瞬間のジョイパッドの入力データが書き込まれ
/// クリアしたらそのデータが読み込み可能になる
let setButtonPressed button isPressed joy =
  let newStatus =
    if isPressed then
      joy.buttonStatus ||| button
    else
      joy.buttonStatus &&& (~~~button)
  { joy with buttonStatus = newStatus }
