module Cpu

open Instructions

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
    P: byte
    Memory: byte array }

let initialMemory = Array.create 0x10000 0uy // 65536バイトを0で初期化

let initialCpu =
  { A = 0uy
    X = 0uy
    Y = 0uy
    PC = 0us
    SP = 0xFFuy
    P = 0uy
    Memory = initialMemory }

let memRead cpu addr = cpu.Memory[int addr]

let memWrite addr data cpu =
  let mem = Array.copy cpu.Memory
  mem[int addr] <- data
  { cpu with Memory = mem }

// 16ビットデータ読み込み（リトルエンディアンをデコード）
let memRead16 cpu pos =
  let read = memRead cpu
  let lo = read  pos        |> uint16
  let hi = read (pos + 1us) |> uint16
  (hi <<< 8) ||| lo

// 16ビットデータ書き込み（リトルエンディアン化）
let memWrite16 (pos: uint16) (data: uint16) cpu =
  let hi = data >>> 8 |> byte
  let lo = data &&& 0xFFus |> byte
  cpu |> memWrite pos lo |> memWrite (pos + 1us) hi

let getOperandAddress cpu pc mode =
  match mode with
  | Accumulator -> failwithf "Unsupported mode: %A" mode
  | Immediate -> pc
  | ZeroPage -> pc |> memRead cpu |> uint16
  | Absolute -> pc |> memRead16 cpu
  | ZeroPage_X ->
    let pos = pc |> memRead cpu
    let addr = pos + cpu.X |> uint16
    addr
  | ZeroPage_Y ->
    let pos = pc |> memRead cpu
    let addr = pos + cpu.Y |> uint16
    addr
  | Absolute_X ->
    let bpos = pc |> memRead16 cpu
    let addr = bpos + (cpu.X |> uint16)
    addr
  | Absolute_Y ->
    let bpos = pc |> memRead16 cpu
    let addr = bpos + (cpu.Y |> uint16)
    addr
  | Indirect_X ->
    let bpos = pc |> memRead cpu
    let ptr = bpos + cpu.X |> uint16
    let addr = ptr |> memRead16 cpu
    addr
  | Indirect_Y ->
    let bpos = pc |> memRead cpu |> uint16
    let deRefBase = bpos |> memRead16 cpu
    let deRef = deRefBase + (cpu.Y |> uint16)
    deRef
  // Indirect でページ境界を指定した場合の動作をよく調べておく
  | Indirect ->
    let bpos = pc |> memRead16 cpu
    let addr = bpos |> memRead16 cpu
    addr
  | Relative ->
    let offset = pc |> memRead cpu |> int8 |> int
    let addr = int pc + offset
    uint16 addr
  | Implied -> pc
  | NoneAddressing -> failwithf "Unsupported mode: %A" mode

let hasFlag flag p = p &&& flag <> 0uy
let setFlag flag p = p ||| flag
let clearFlag flag p = p &&& ~~~flag
let updateFlag flag condition p =
  if condition then setFlag flag p else clearFlag flag p

// 値によってZNフラグをセットする
let setZeroNegativeFlags value p = 
  p
  |> updateFlag Flags.Z (value = 0uy)
  |> updateFlag Flags.N (value &&& 0b1000_0000uy <> 0uy)
let advancePC offset cpu =
  { cpu with PC = cpu.PC + offset }
// ビット論理演算
let logicalInstr op mode cpu = // AND, EOR, ORA
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let value = addr |> memRead cpu

  let a =
    match op with
    | AND -> (&&&) cpu.A value
    | EOR -> (^^^) cpu.A value
    | ORA -> (|||) cpu.A value
    | _   -> failwithf "Unsupported logical instruction: %A" op

  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }

// 加算
let adc mode cpu = // Add with Carry
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let value = addr |> memRead cpu
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

  { cpu with A = result; P = p }

// 減算
let sbc mode cpu = // Subtract with Carry
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let value = addr |> memRead cpu
  let carry = if hasFlag Flags.C cpu.P then 1 else 0

  let valueInverted = value ^^^ 0xFFuy
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

  { cpu with A = result; P = p }

let lda mode cpu = // Load Accumulator
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let a = addr |> memRead cpu
  let p = setZeroNegativeFlags a cpu.P
  { cpu with A = a; P = p }

let ldx mode cpu = // Load X Register
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let x = addr |> memRead cpu
  let p = setZeroNegativeFlags x cpu.P
  { cpu with X = x; P = p }

let ldy mode cpu = // Load Y Register
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let y = addr |> memRead cpu
  let p = setZeroNegativeFlags y cpu.P
  { cpu with Y = y; P = p }
let sta mode cpu = // Store Accumulator
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  cpu |> memWrite addr cpu.A
let stx mode cpu = // Store X Register
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  cpu |> memWrite addr cpu.X
let sty mode cpu = // Store Y Register
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  cpu |> memWrite addr cpu.Y

let tax _ cpu = // Transfer Accumulator to X
  let x = cpu.A
  let p = cpu.P |> setZeroNegativeFlags x
  { cpu with X = x; P = p }
let tay _ cpu = // Transfer Accumulator to Y
  let y = cpu.A
  let p = cpu.P |> setZeroNegativeFlags y
  { cpu with Y = y; P = p }
let txa _ cpu = // Transfer X to Accumulator
  let a = cpu.X
  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }
let tya _ cpu = // Transfer Y to Accumulator
  let a = cpu.Y
  let p = cpu.P |> setZeroNegativeFlags a
  { cpu with A = a; P = p }
let tsx _ cpu = // Transfer Stack Pointer to X
  let x = cpu.SP
  let p = cpu.P |> setZeroNegativeFlags x
  { cpu with X = x; P = p }
let txs _ cpu = // Transfer X to Stack Pointer
  let sp = cpu.X
  { cpu with SP = sp }
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

let modifyWithShift mode cpu shiftFn carryInFn =
  let cIn = hasFlag Flags.C cpu.P

  let value, writeBack =
    match mode with
    | Accumulator ->
        cpu.A, fun r -> { cpu with A = r }
    | _ ->
        let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
        let v = addr |> memRead cpu 
        v, fun r -> cpu |> memWrite addr r

  let shifted, p = shiftFn value cpu.P
  let result = carryInFn shifted cIn
  let p' = setZeroNegativeFlags result p
  writeBack result |> fun c -> { c with P = p' }

let asl mode cpu =
  modifyWithShift mode cpu shiftLeftAndUpdate (fun r _ -> r)

let lsrInstr mode cpu =
  modifyWithShift mode cpu shiftRightAndUpdate (fun r _ -> r)

let rol mode cpu =
  modifyWithShift mode cpu shiftLeftAndUpdate (fun r carryBefore ->
    r ||| (if carryBefore then 1uy else 0uy))

let ror mode cpu =
  modifyWithShift mode cpu shiftRightAndUpdate (fun r carryBefore ->
    r ||| (if carryBefore then 0b1000_0000uy else 0uy))


// 分岐はこの関数内で PC の進みを管理する
let branch mode flag expected cpu =
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us) |> (+) 1us // 命令の分進める
  if hasFlag flag cpu.P = expected then { cpu with PC = addr } else cpu |> advancePC 2us 
let bcc mode cpu = // BCC - Branch if Carry Clear
  // cpu |> branch mode Flags.C false
  cpu |> branch mode Flags.C false
let bcs mode cpu = // BCS - Branch if Carry Set
  cpu |> branch mode Flags.C true

let beq mode cpu = // BEQ - Branch if Equal
  cpu |> branch mode Flags.Z true

let bne mode cpu = // BNE - Branch if Not Equal
  cpu |> branch mode Flags.Z false

let bmi mode cpu = // Branch if Minus
  cpu |> branch mode Flags.N true

let bpl mode cpu = // Branch if Positive
  cpu |> branch mode Flags.N false

let bvc mode cpu = // Branch if Overflow Clear
  cpu |> branch mode Flags.V false

let bvs mode cpu = // Branch if Overflow Set
  cpu |> branch mode Flags.V true

let clc _ cpu = // Clear Carry Flag
  let p = clearFlag Flags.C cpu.P
  { cpu with P = p }

let sec _ cpu = // Set Carry Flag
  let p = setFlag Flags.C cpu.P
  { cpu with P = p }

let cld _ cpu = // Clear Decimal Mode
  let p = clearFlag Flags.D cpu.P
  { cpu with P = p }

let sed _ cpu = // Set Decimal Flag
  let p = setFlag Flags.D cpu.P
  { cpu with P = p }

let cli _ cpu = // Clear Interrupt Disable
  let p = clearFlag Flags.I cpu.P
  { cpu with P = p }

let sei _ cpu = // Set Interrupt Disable
  let p = setFlag Flags.I cpu.P
  { cpu with P = p }

let clv _ cpu = // Clear Overflow Flag
  let p = clearFlag Flags.V cpu.P
  { cpu with P = p }

let bit mode cpu = // BIT - Bit Test
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let value = addr |> memRead cpu
  let p =
    cpu.P
    |> updateFlag Flags.Z (cpu.A &&& value    = 0uy)
    |> updateFlag Flags.N (value &&& Flags.N <> 0uy)
    |> updateFlag Flags.V (value &&& Flags.V <> 0uy)
  { cpu with P = p }

let compareInstr lhs mode cpu =
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let value = addr |> memRead cpu
  let result = lhs - value
  let p =
    cpu.P
    |> updateFlag Flags.C (lhs >= value)
    |> setZeroNegativeFlags result
  { cpu with P = p }

let cmp mode cpu = // Compare
  cpu |> compareInstr cpu.A mode

let cpx mode cpu = // Compare X Register
  cpu |> compareInstr cpu.X mode

let cpy mode cpu = // Compare Y Register
  cpu |> compareInstr cpu.Y mode

let dec mode cpu = // Decrement Memory
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let result = (addr |> memRead cpu) - 1uy
  cpu |> memWrite addr result
      |> fun c -> { c with P = cpu.P |> setZeroNegativeFlags result }
let dex _ cpu = // Decrement X Register
  let x = cpu.X - 1uy
  { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }

let dey _ cpu = // Decrement X Register
  let y = cpu.Y - 1uy
  { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }

let inc mode cpu = // Decrement Memory
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  let result = (addr |> memRead cpu) + 1uy
  cpu |> memWrite addr result
      |> fun c -> { c with P = cpu.P |> setZeroNegativeFlags result }
let inx _ cpu =
  let x = cpu.X + 1uy
  { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }

let iny _ cpu = // Decrement X Register
  let y = cpu.Y + 1uy
  { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }
let jmp mode cpu = // Jump
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  { cpu with PC = addr }

let push value cpu = // Push value to stack
  let addr = 0x0100us + uint16 cpu.SP
  cpu |> memWrite addr value
      |> fun c -> { c with SP = c.SP - 1uy }
let push16 value cpu = // Push 16-bit value to stack
  let addr = 0x0100us + uint16 cpu.SP - 1us
  cpu |> memWrite16 addr value
      |> fun c -> { c with SP = c.SP - 2uy }
let pull cpu = // Pull value from stack
  let sp = cpu.SP + 1uy
  let addr = 0x0100us + uint16 sp
  let value = addr |> memRead cpu
  { cpu with SP = sp }, value

let pull16 cpu = // Pull 16-bit value from stack
  let addr = 0x0100us + uint16 cpu.SP + 1us
  let sp = cpu.SP + 2uy
  let value = addr |> memRead16 cpu
  { cpu with SP = sp }, value

let jsr mode cpu = // Jump to Subroutine
  let addr = mode |> getOperandAddress cpu (cpu.PC + 1us)
  cpu |> push16 (cpu.PC + 2us) // リターンポイント -1
      |> fun c -> { c with PC = addr }

let rts _ cpu = // Return from Subroutine
  let cpu, pc = pull16 cpu
  { cpu with PC = pc + 1us } // +1 してリターンポイントにする

let pha _ cpu = // Push Accumulator
  cpu |> push cpu.A
let php _ cpu = // Push Processor Status
  cpu |> push cpu.P
let plp _ cpu = // Pull Processor Status
  let c, p = cpu |> pull
  { c with P = p }
let pla _ cpu = // Pull Accumulator
  let c, a = cpu |> pull
  { c with A = a; P = c.P |> setZeroNegativeFlags a }

let brk _ cpu = // BRK - Force Break
  // まだ未完成
  // cpu |> push16 (cpu.PC + 1us)
  //     |> push cpu.P
  //     |> fun c -> { c with
  //                     PC = memRead16 cpu 0xFFFEus
  //                     P = c.P |> updateFlag Flags.B true
  //                 } // BRK の場合は 0xFFFE に飛ぶ
  { cpu with P = cpu.P |> updateFlag Flags.B true }
let rti _ cpu = // Return from Interrupt
  let c, p = cpu |> pull
  let c, pc = c |> pull16
  { c with P = p; PC = pc }

let nop _ cpu = // No Operation
  cpu
let reset cpu =
  { cpu with
      A = 0uy
      X = 0uy
      Y = 0uy
      P = 0uy
      PC = 0xFFFCus |> memRead16 cpu
      // todo?: Memory
  }

let load program cpu =
  let mem = Array.copy cpu.Memory
  // for NES
  // Array.blit program 0 mem 0x8000 program.Length
  // let cpu = { cpu with Memory = mem }
  // cpu |> memWrite16 0xFFFCus 0x8000us
  // for Snake game
  Array.blit program 0 mem 0x0600 program.Length
  let cpu = { cpu with Memory = mem }
  cpu |> memWrite16 0xFFFCus 0x0600us

    /// CPUを1命令だけ実行する（既存の関数があればそれを使用）
let step cpu =
    let opcode = cpu.PC |> memRead cpu
    let op, mode, size, cycles = decodeOpcode opcode
    let execInstr f m c =
      c |> f m |> advancePC size
    match op with
      | ADC -> cpu |> execInstr adc mode
      | AND -> cpu |> execInstr (logicalInstr AND) mode
      | ASL -> cpu |> execInstr asl mode
      | BCC -> cpu |> bcc mode
      | BCS -> cpu |> bcs mode
      | BEQ -> cpu |> beq mode
      | BIT -> cpu |> execInstr bit mode
      | BMI -> cpu |> bmi mode
      | BNE -> cpu |> bne mode
      | BPL -> cpu |> bpl mode
      | BRK -> cpu |> execInstr brk mode
      | BVC -> cpu |> bvc mode
      | BVS -> cpu |> bvs mode
      | CLC -> cpu |> execInstr clc mode
      | CLD -> cpu |> execInstr cld mode
      | CLI -> cpu |> execInstr cli mode
      | CLV -> cpu |> execInstr clv mode
      | CMP -> cpu |> execInstr cmp mode
      | CPX -> cpu |> execInstr cpx mode
      | CPY -> cpu |> execInstr cpy mode
      | DEC -> cpu |> execInstr dec mode
      | DEX -> cpu |> execInstr dex mode
      | DEY -> cpu |> execInstr dey mode
      | EOR -> cpu |> execInstr (logicalInstr EOR) mode
      | INC -> cpu |> execInstr inc mode
      | INX -> cpu |> execInstr inx mode
      | INY -> cpu |> execInstr iny mode
      | JMP -> cpu |> jmp mode
      | JSR -> cpu |> jsr mode
      | LDA -> cpu |> execInstr lda mode
      | LDX -> cpu |> execInstr ldx mode
      | LDY -> cpu |> execInstr ldy mode
      | LSR -> cpu |> execInstr lsrInstr mode
      | NOP -> cpu |> execInstr nop mode
      | ORA -> cpu |> execInstr (logicalInstr ORA) mode
      | PHA -> cpu |> execInstr pha mode
      | PHP -> cpu |> execInstr php mode
      | PLA -> cpu |> execInstr pla mode
      | PLP -> cpu |> execInstr plp mode
      | ROL -> cpu |> execInstr rol mode
      | ROR -> cpu |> execInstr ror mode
      | SEC -> cpu |> execInstr sec mode
      | SED -> cpu |> execInstr sed mode
      | SEI -> cpu |> execInstr sei mode
      | SBC -> cpu |> execInstr sbc mode
      | STA -> cpu |> execInstr sta mode
      | STX -> cpu |> execInstr stx mode
      | STY -> cpu |> execInstr sty mode
      | RTI -> cpu |> rti mode
      | RTS -> cpu |> rts mode
      | TAX -> cpu |> execInstr tax mode
      | TAY -> cpu |> execInstr tay mode
      | TSX -> cpu |> execInstr tsx mode
      | TXS -> cpu |> execInstr txs mode
      | TXA -> cpu |> execInstr txa mode
      | TYA -> cpu |> execInstr tya mode
      | _ -> failwithf "Unsupported mnemonic: %A" op

let rec run cpu =
  let c = cpu |> step
  // if c.P |> hasFlag Flags.B then c else c |> run
  // BRK に当たったときループを抜ける暫定処理
  if c.P |> hasFlag Flags.B then c else c |> run

