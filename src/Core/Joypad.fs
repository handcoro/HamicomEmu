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
  strobe: bool
  buttonIdx: int
  buttonStatus: byte
}

let initialJoypad = {
  strobe = false
  buttonIdx = 0
  buttonStatus = 0uy
}

let writeJoypad value joy =
  let strobe = if value &&& 1uy <> 0uy then true else false
  let idx = if strobe then 0 else joy.buttonIdx
  { joy with strobe = strobe; buttonIdx = idx }

let readJoypad joy =
  let idx = joy.buttonIdx
  if idx > 7 then
    1uy, joy
  else
    let bit = (joy.buttonStatus >>> idx) &&& 1uy
    let idx' = if not joy.strobe && idx <= 7 then idx + 1 else idx
    bit, { joy with buttonIdx = idx'}

let setButtonPressed button isPressed joy =
  let newStatus =
    if isPressed then
      joy.buttonStatus ||| button
    else
      joy.buttonStatus &&& (~~~button)
  { joy with buttonStatus = newStatus }
