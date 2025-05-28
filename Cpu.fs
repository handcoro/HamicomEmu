module Cpu

open Instructions
open Bus

module Flags =
  let C = 0b0000_0001uy // Carry
  let Z = 0b0000_0010uy // Zero
  let I = 0b0000_0100uy // Interrupt Disable
  let D = 0b0000_1000uy // Decimal Mode
  let B = 0b0001_0000uy // Break
  let U = 0b0010_0000uy // Unused (常にセットされることが多い)
  let V = 0b0100_0000uy // Overflow
  let N = 0b1000_0000uy // Negative

// PC: program counter
// SP: stack pointer
// P: processor status (flags)
type CpuState =
  { A: byte
    X: byte
    Y: byte
    PC: uint16
    SP: byte
    P: byte }

let initialCpu =
  { A = 0uy
    X = 0uy
    Y = 0uy
    PC = 0us
    SP = 0xFDuy
    P = Flags.I ||| Flags.U }

let getOperandAddress cpu bus pc mode =
  match mode with
  | Accumulator ->
    failwithf "Unsupported mode: %A" mode
  | Immediate ->
    pc
  | ZeroPage ->
    let addr, _ = memRead pc bus
    addr |> uint16
  | Absolute ->
    let addr, _ = memRead16 pc bus
    addr |> uint16
  | ZeroPage_X ->
    let pos, _ = memRead pc bus
    let addr = pos + cpu.X |> uint16
    addr
  | ZeroPage_Y ->
    let pos, _ = memRead pc bus
    let addr = pos + cpu.Y |> uint16
    addr
  | Absolute_X ->
    let bpos, _ = memRead16 pc bus
    let addr = bpos + (cpu.X |> uint16)
    addr
  | Absolute_Y ->
    let bpos, _ = memRead16 pc bus
    let addr = bpos + (cpu.Y |> uint16)
    addr
  | Indirect_X ->
    let bpos, _ = memRead pc bus
    let ptr = bpos + cpu.X
    let addr, _ = memRead16ZeroPage ptr bus
    addr
  | Indirect_Y ->
    let bpos, _ = memRead pc bus
    let deRefBase, _ = memRead16ZeroPage bpos bus
    let deRef = deRefBase + (cpu.Y |> uint16)
    deRef
  | Indirect ->
    let bpos, _ = memRead16 pc bus
    let addr, _ = memRead16Wrap bpos bus // JMP のページ境界バグ
    addr
  | Relative ->
    let offset, _ = memRead pc bus
    let addr = int pc + (offset |> int8 |> int)
    uint16 addr
  | Implied ->
    pc
  // | NoneAddressing ->
  //   failwithf "Unsupported mode: %A" mode

let inline hasFlag flag p = p &&& flag <> 0uy
let inline setFlag flag p = p ||| flag
let inline clearFlag flag p = p &&& ~~~flag
let updateFlag flag condition p =
  if condition then setFlag flag p else clearFlag flag p

// 値によってZNフラグをセットする
let setZeroNegativeFlags value p = 
  p
  |> updateFlag Flags.Z (value = 0uy)
  |> updateFlag Flags.N (value &&& 0b1000_0000uy <> 0uy)
let advancePC offset cpu bus =
  { cpu with PC = cpu.PC + offset }, bus

// ビット論理演算
let logicalInstr op mode cpu bus = // AND, EOR, ORA
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value, bus' = memRead addr bus

  let a =
    match op with
    | AND -> (&&&) cpu.A value
    | EOR -> (^^^) cpu.A value
    | ORA -> (|||) cpu.A value
    | _   -> failwithf "Unsupported logical instruction: %A" op

  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }, bus'

// 加算
let adc mode cpu bus = // Add with Carry
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value, bus' = memRead addr bus
  let carry = if hasFlag Flags.C cpu.P then 1 else 0
  let sum = int cpu.A + int value + carry
  let result = byte sum
  let isCarry = sum > 0xFF
  let isOverflow =
    // オーバーフローが発生する条件:
    // 1. A と value の符号が同じ
    // 2. 結果の符号が A と異なる
    cpu.A ^^^ value &&& 0x80uy = 0uy && cpu.A ^^^ result &&& 0x80uy <> 0uy

  let p =
    cpu.P
    |> updateFlag Flags.C isCarry
    |> updateFlag Flags.V isOverflow
    |> setZeroNegativeFlags result

  { cpu with A = result; P = p }, bus'

// 減算
let sbc mode cpu bus = // Subtract with Carry
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value, bus' = memRead addr bus
  let carry = if hasFlag Flags.C cpu.P then 1 else 0
  let valueInverted = ~~~ value
  let sum = int cpu.A + int valueInverted + carry
  let result = byte sum
  let isCarry = sum > 0xFF
  let isOverflow =
    // ADC とオーバーフローの条件が違うので注意
    cpu.A ^^^ value &&& 0x80uy <> 0uy && cpu.A ^^^ result &&& 0x80uy <> 0uy

  let p =
    cpu.P
    |> updateFlag Flags.C isCarry
    |> updateFlag Flags.V isOverflow
    |> setZeroNegativeFlags result

  { cpu with A = result; P = p }, bus'

let lda mode cpu bus = // Load Accumulator
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let a, bus' = memRead addr bus
  let p = setZeroNegativeFlags a cpu.P
  { cpu with A = a; P = p }, bus'

let ldx mode cpu bus = // Load X Register
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let x, bus' = memRead addr bus
  let p = setZeroNegativeFlags x cpu.P
  { cpu with X = x; P = p }, bus'

let ldy mode cpu bus = // Load Y Register
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let y, bus' = memRead addr bus
  let p = setZeroNegativeFlags y cpu.P
  { cpu with Y = y; P = p }, bus'

let sta mode cpu bus = // Store Accumulator
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  cpu, bus |> memWrite addr cpu.A

let stx mode cpu bus = // Store X Register
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  cpu, bus |> memWrite addr cpu.X

let sty mode cpu bus = // Store Y Register
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  cpu, bus |> memWrite addr cpu.Y

let tax _ cpu bus = // Transfer Accumulator to X
  let x = cpu.A
  let p = cpu.P |> setZeroNegativeFlags x
  { cpu with X = x; P = p }, bus

let tay _ cpu bus = // Transfer Accumulator to Y
  let y = cpu.A
  let p = cpu.P |> setZeroNegativeFlags y
  { cpu with Y = y; P = p }, bus

let txa _ cpu bus = // Transfer X to Accumulator
  let a = cpu.X
  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }, bus

let tya _ cpu bus = // Transfer Y to Accumulator
  let a = cpu.Y
  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }, bus

let tsx _ cpu bus = // Transfer Stack Pointer to X
  let x = cpu.SP
  let p = cpu.P |> setZeroNegativeFlags x
  { cpu with X = x; P = p }, bus

let txs _ cpu bus = // Transfer X to Stack Pointer
  let sp = cpu.X
  { cpu with SP = sp }, bus

let shiftLeftAndUpdate value status =
  let carry = value &&& 0b1000_0000uy > 0uy
  let result = value <<< 1
  let p =
    status
    |> updateFlag Flags.C carry
  result, p

let shiftRightAndUpdate value status =
  let carry = value &&& 0b0000_0001uy > 0uy
  let result = value >>> 1
  let p =
    status
    |> updateFlag Flags.C carry
  result, p

let modifyWithShift mode shiftFn carryInFn cpu bus =
  let cIn = hasFlag Flags.C cpu.P

  let value, writeBack =
    match mode with
    | Accumulator ->
        cpu.A, fun r -> { cpu with A = r }, bus
    | _ ->
        let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
        let v, b' = memRead addr bus
        v, fun r -> cpu, b' |> memWrite addr r

  let shifted, p = shiftFn value cpu.P
  let result = carryInFn shifted cIn
  let p' = setZeroNegativeFlags result p
  let cpu', bus' = writeBack result
  { cpu' with P = p' }, bus'

let asl mode cpu bus =
  (cpu, bus) ||> modifyWithShift mode shiftLeftAndUpdate (fun r _ -> r)

let lsrInstr mode cpu bus =
  (cpu, bus) ||> modifyWithShift mode shiftRightAndUpdate (fun r _ -> r)

let rol mode cpu bus =
  (cpu, bus) ||> modifyWithShift mode shiftLeftAndUpdate (fun r carryBefore ->
    r ||| (if carryBefore then 1uy else 0uy))

let ror mode cpu bus =
  (cpu, bus) ||> modifyWithShift mode shiftRightAndUpdate (fun r carryBefore ->
    r ||| (if carryBefore then 0b1000_0000uy else 0uy))
// 分岐はこの関数内で PC の進みを管理する
let branch mode flag expected cpu bus =
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us) |> (+) 1us // 命令の分進める
  if hasFlag flag cpu.P = expected then { cpu with PC = addr }, bus else (cpu, bus) ||> advancePC 2us
let bcc mode (cpu : CpuState) (bus : Bus) = // BCC - Branch if Carry Clear
  (cpu, bus) ||> branch mode Flags.C false
let bcs mode cpu bus = // BCS - Branch if Carry Set
  (cpu, bus) ||> branch mode Flags.C true

let beq mode cpu bus = // BEQ - Branch if Equal
  (cpu, bus) ||> branch mode Flags.Z true

let bne mode cpu bus = // BNE - Branch if Not Equal
  (cpu, bus) ||> branch mode Flags.Z false

let bmi mode cpu bus = // Branch if Minus
  (cpu, bus) ||> branch mode Flags.N true

let bpl mode cpu bus = // Branch if Positive
  (cpu, bus) ||> branch mode Flags.N false

let bvc mode cpu bus = // Branch if Overflow Clear
  (cpu, bus) ||> branch mode Flags.V false

let bvs mode cpu bus = // Branch if Overflow Set
  (cpu, bus) ||> branch mode Flags.V true

let clc _ cpu bus = // Clear Carry Flag
  let p = clearFlag Flags.C cpu.P
  { cpu with P = p }, bus

let sec _ cpu bus = // Set Carry Flag
  let p = setFlag Flags.C cpu.P
  { cpu with P = p }, bus

let cld _ cpu bus = // Clear Decimal Mode
  let p = clearFlag Flags.D cpu.P
  { cpu with P = p }, bus

let sed _ cpu bus = // Set Decimal Flag
  let p = setFlag Flags.D cpu.P
  { cpu with P = p }, bus

let cli _ cpu bus = // Clear Interrupt Disable
  let p = clearFlag Flags.I cpu.P
  { cpu with P = p }, bus

let sei _ cpu bus = // Set Interrupt Disable
  let p = setFlag Flags.I cpu.P
  { cpu with P = p }, bus

let clv _ cpu bus = // Clear Overflow Flag
  let p = clearFlag Flags.V cpu.P
  { cpu with P = p }, bus

let bit mode cpu bus = // BIT - Bit Test
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value, bus' = memRead addr bus
  let p =
    cpu.P
    |> updateFlag Flags.Z (cpu.A &&& value    = 0uy)
    |> updateFlag Flags.N (value &&& Flags.N <> 0uy)
    |> updateFlag Flags.V (value &&& Flags.V <> 0uy)
  { cpu with P = p }, bus'

let compareInstr lhs mode cpu bus =
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value, bus' = memRead addr bus
  let result = lhs - value
  let p =
    cpu.P
    |> updateFlag Flags.C (lhs >= value)
    |> setZeroNegativeFlags result
  { cpu with P = p }, bus'

let cmp mode cpu bus = // Compare
  (cpu, bus) ||> compareInstr cpu.A mode

let cpx mode cpu bus = // Compare X Register
  (cpu, bus) ||> compareInstr cpu.X mode

let cpy mode cpu bus = // Compare Y Register
  (cpu, bus) ||> compareInstr cpu.Y mode

let dec mode cpu bus = // Decrement Memory
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let result, bus' = memRead addr bus
  cpu |> fun c -> { c with P = cpu.P |> setZeroNegativeFlags (result - 1uy) }, bus' |> memWrite addr (result - 1uy)

let dex _ cpu bus = // Decrement X Register
  let x = cpu.X - 1uy
  { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }, bus

let dey _ cpu bus = // Decrement Y Register
  let y = cpu.Y - 1uy
  { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }, bus

let inc mode cpu bus = // Increment Memory
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let result, bus' = memRead addr bus
  { cpu with P = cpu.P |> setZeroNegativeFlags (result + 1uy) }, bus' |> memWrite addr (result + 1uy)

let inx _ cpu bus = // Increment X Register
  let x = cpu.X + 1uy
  { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }, bus

let iny _ cpu bus = // Increment Y Register
  let y = cpu.Y + 1uy
  { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }, bus

let jmp mode cpu bus = // Jump
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  { cpu with PC = addr }, bus

let push value cpu bus = // Push value to stack
  let addr = 0x0100us + uint16 cpu.SP
  { cpu with SP = cpu.SP - 1uy }, bus |> memWrite addr value

let push16 value cpu bus = // Push 16-bit value to stack
  let addr = 0x0100us + uint16 cpu.SP - 1us
  { cpu with SP = cpu.SP - 2uy }, bus |> memWrite16 addr value

let pull cpu bus = // Pull value from stack
  let sp = cpu.SP + 1uy
  let addr = 0x0100us + uint16 sp
  let value, bus' = memRead addr bus
  value, { cpu with SP = sp }, bus'

let pull16 cpu bus = // Pull 16-bit value from stack
  let addr = 0x0100us + uint16 cpu.SP + 1us
  let sp = cpu.SP + 2uy
  let value, bus' = memRead16 addr bus
  value, { cpu with SP = sp }, bus'

let jsr mode cpu bus = // Jump to Subroutine
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let cpu', bus' = (cpu, bus) ||> push16 (cpu.PC + 2us) // リターンポイント -1
  { cpu' with PC = addr }, bus'

let rts _ cpu bus = // Return from Subroutine
  let pc, cpu', bus' = pull16 cpu bus
  { cpu' with PC = pc + 1us }, bus' // +1 してリターンポイントにする

let pha _ cpu bus = // Push Accumulator
  (cpu, bus) ||> push cpu.A

let php _ cpu bus = // Push Processor Status
  (cpu, bus) ||> push (cpu.P |> setFlag Flags.B ||| Flags.U) // B フラグをセットしてプッシュ

let plp _ cpu bus = // Pull Processor Status
  let p, cpu', bus' = pull cpu bus
  { cpu' with P = p |> clearFlag Flags.B |> setFlag Flags.U }, bus'

let pla _ cpu bus = // Pull Accumulator
  let a, cpu', bus' = pull cpu bus
  { cpu' with A = a; P = cpu'.P |> setZeroNegativeFlags a }, bus'

let brk _ cpu bus = // BRK - Force Break
  // まだ未完成
  // cpu |> push16 (cpu.PC + 1us)
  //     |> push cpu.P
  //     |> fun c -> { c with
  //                     PC = memRead16 cpu 0xFFFEus
  //                     P = c.P |> updateFlag Flags.B true
  //                 } // BRK の場合は 0xFFFE に飛ぶ
  { cpu with P = cpu.P |> updateFlag Flags.B true }, bus

let rti _ cpu bus = // Return from Interrupt
  let p, cpu', bus' = pull cpu bus
  let pc, cpu'', bus'' = pull16 cpu' bus'
  { cpu'' with P = p |> clearFlag Flags.B |> setFlag Flags.U; PC = pc }, bus''

let nop _ cpu bus = // No Operation
  cpu, bus

let lax mode cpu bus = // LDA -> TAX
  (cpu, bus) ||> lda mode ||> tax mode

let sax mode cpu bus = // Store A and X
  let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
  let value = cpu.A &&& cpu.X
  cpu, bus |> memWrite addr value

let dcp mode cpu bus = // DEC -> CMP
  (cpu, bus) ||> dec mode ||> cmp mode

let isb mode cpu bus = // (ISC) INC -> SBC
  (cpu, bus) ||> inc mode ||> sbc mode

let slo mode cpu bus = // ASL -> ORA
  (cpu, bus) ||> asl mode ||> logicalInstr ORA mode

let rla mode cpu bus =
  (cpu, bus) ||> rol mode ||> logicalInstr AND mode

let rra mode cpu bus =
  (cpu, bus) ||> ror mode ||> adc mode

let sre mode cpu bus =
  (cpu, bus) ||> lsrInstr mode ||> logicalInstr EOR mode

let interruptNmi cpu bus =
  let cpu', bus' = push16 cpu.PC cpu bus
  let p = cpu.P |> updateFlag Flags.B false
  let p' = p |> updateFlag Flags.U true

  let cpu2, bus2 = push p' cpu' bus'
  let p2 = p' |> updateFlag Flags.I true

  let bus3 = tick 2u bus2
  let pc, busF = memRead16 0xFFFAus bus3
  { cpu2 with P = p2; PC = pc }, busF


let reset cpu bus =
  let pc, bus' = memRead16 0xFFFCus bus // リセットベクタを読み込む
  { cpu with
      A = 0uy
      X = 0uy
      Y = 0uy
      P = 0uy
      PC = pc 
  }, bus'

// let load program cpu bus =
//   let mem = Array.copy cpu.Memory
//   Array.blit program 0 mem 0x8000 program.Length
//   let cpu = { cpu with Memory = mem }
//   cpu, bus |> memWrite16 0xFFFCus 0x8000us

// let loadSnake program cpu bus =
//   let mem = Array.copy cpu.Memory
//   Array.blit program 0 mem 0x0600 program.Length
//   let cpu = { cpu with Memory = mem }
//   cpu, bus |> memWrite16 0xFFFCus 0x0600us

    /// CPU を 1 命令だけ実行する
let step cpu bus : CpuState * Bus =
    let opcode, bus' = memRead cpu.PC bus
    let op, mode, size, cycles = decodeOpcode opcode
    let execInstr f m c b =
      (c, b) ||> f m ||> advancePC size
    match op with
      | ADC -> (cpu, bus') ||> execInstr adc mode
      | AND -> (cpu, bus') ||> execInstr (logicalInstr AND) mode
      | ASL -> (cpu, bus') ||> execInstr asl mode
      | BCC -> (cpu, bus') ||> bcc mode
      | BCS -> (cpu, bus') ||> bcs mode
      | BEQ -> (cpu, bus') ||> beq mode
      | BIT -> (cpu, bus') ||> execInstr bit mode
      | BMI -> (cpu, bus') ||> bmi mode
      | BNE -> (cpu, bus') ||> bne mode
      | BPL -> (cpu, bus') ||> bpl mode
      | BRK -> (cpu, bus') ||> execInstr brk mode
      | BVC -> (cpu, bus') ||> bvc mode
      | BVS -> (cpu, bus') ||> bvs mode
      | CLC -> (cpu, bus') ||> execInstr clc mode
      | CLD -> (cpu, bus') ||> execInstr cld mode
      | CLI -> (cpu, bus') ||> execInstr cli mode
      | CLV -> (cpu, bus') ||> execInstr clv mode
      | CMP -> (cpu, bus') ||> execInstr cmp mode
      | CPX -> (cpu, bus') ||> execInstr cpx mode
      | CPY -> (cpu, bus') ||> execInstr cpy mode
      | DEC -> (cpu, bus') ||> execInstr dec mode
      | DEX -> (cpu, bus') ||> execInstr dex mode
      | DEY -> (cpu, bus') ||> execInstr dey mode
      | EOR -> (cpu, bus') ||> execInstr (logicalInstr EOR) mode
      | INC -> (cpu, bus') ||> execInstr inc mode
      | INX -> (cpu, bus') ||> execInstr inx mode
      | INY -> (cpu, bus') ||> execInstr iny mode
      | JMP -> (cpu, bus') ||> jmp mode
      | JSR -> (cpu, bus') ||> jsr mode
      | LDA -> (cpu, bus') ||> execInstr lda mode
      | LDX -> (cpu, bus') ||> execInstr ldx mode
      | LDY -> (cpu, bus') ||> execInstr ldy mode
      | LSR -> (cpu, bus') ||> execInstr lsrInstr mode
      | NOP | NOP_ -> (cpu, bus') ||> execInstr nop mode
      | ORA -> (cpu, bus') ||> execInstr (logicalInstr ORA) mode
      | PHA -> (cpu, bus') ||> execInstr pha mode
      | PHP -> (cpu, bus') ||> execInstr php mode
      | PLA -> (cpu, bus') ||> execInstr pla mode
      | PLP -> (cpu, bus') ||> execInstr plp mode
      | ROL -> (cpu, bus') ||> execInstr rol mode
      | ROR -> (cpu, bus') ||> execInstr ror mode
      | SEC -> (cpu, bus') ||> execInstr sec mode
      | SED -> (cpu, bus') ||> execInstr sed mode
      | SEI -> (cpu, bus') ||> execInstr sei mode
      | SBC | SBC_ -> (cpu, bus') ||> execInstr sbc mode
      | STA -> (cpu, bus') ||> execInstr sta mode
      | STX -> (cpu, bus') ||> execInstr stx mode
      | STY -> (cpu, bus') ||> execInstr sty mode
      | RTI -> (cpu, bus') ||> rti mode
      | RTS -> (cpu, bus') ||> rts mode
      | TAX -> (cpu, bus') ||> execInstr tax mode
      | TAY -> (cpu, bus') ||> execInstr tay mode
      | TSX -> (cpu, bus') ||> execInstr tsx mode
      | TXS -> (cpu, bus') ||> execInstr txs mode
      | TXA -> (cpu, bus') ||> execInstr txa mode
      | TYA -> (cpu, bus') ||> execInstr tya mode
      | LAX_ -> (cpu, bus') ||> execInstr lax mode
      | SAX_ -> (cpu, bus') ||> execInstr sax mode
      | DCP_ -> (cpu, bus') ||> execInstr dcp mode
      | ISB_ -> (cpu, bus') ||> execInstr isb mode
      | SLO_ -> (cpu, bus') ||> execInstr slo mode
      | RLA_ -> (cpu, bus') ||> execInstr rla mode
      | RRA_ -> (cpu, bus') ||> execInstr rra mode
      | SRE_ -> (cpu, bus') ||> execInstr sre mode
      | _ -> failwithf "Unsupported mnemonic: %A" op

let rec run cpu bus =
  let cpu', bus' = (cpu, bus) ||> step
  // BRK に当たったときループを抜ける暫定処理
  if hasFlag Flags.B cpu'.P then cpu', bus' else (cpu', bus') ||> run

let rec runWithCallback callback cpu bus =
  callback cpu bus
  let cpu', bus' = (cpu, bus) ||> step
  (cpu', bus') ||> runWithCallback callback