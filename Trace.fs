module Trace

open Instructions
open Cpu
open Bus

open Microsoft.FSharp.Reflection

(*
  imm: #$xx
  zp:  $xx
  abs: $xxxx
  zpx: $xx,X
  zpy: $xx,Y
  abx: $xxxx,X
  aby: $xxxx,Y
  inx: ($xx,X)
  iny: ($xx),Y
  ind: $xxxx

*)
let formatAddress pc mode (args: byte[]) =
  match mode with
  | Accumulator ->
    "A"
  | Immediate ->
    sprintf "#$%02X" args[0]
  | ZeroPage ->
    sprintf "$%02X" args[0]
  | ZeroPage_X ->
    sprintf "$%02X,X" args[0]
  | ZeroPage_Y ->
    sprintf "$%02X,Y" args[0]
  | Absolute ->
    sprintf "$%02X%02X" args[1] args[0]
  | Absolute_X ->
    sprintf "$%02X%02X,X" args[1] args[0]
  | Absolute_Y ->
    sprintf "$%02X%02X,Y" args[1] args[0]
  | Indirect_X ->
    sprintf "($%02X,X)" args[0]
  | Indirect_Y ->
    sprintf "($%02X),Y" args[0]
  | Indirect ->
    sprintf "($%02X%02X)" args[1] args[0]
  | Relative ->
    sprintf "$%04X" (pc + uint16 args[0] + 2us)
  | Implied ->
    ""
  | NoneAddressing ->
    failwithf "Unsupported mode: %A" mode

let formatMemoryAccess cpu bus op mode (args: byte[]) =
  match mode with
  | ZeroPage ->
    let addr = args[0] |> uint16
    let value = addr |> memRead bus
    sprintf "= %02X" value
  | ZeroPage_X ->
    let addr = args[0] + cpu.X |> uint16
    let value = addr |> memRead bus
    sprintf "@ %02X = %02X" addr value
  | ZeroPage_Y ->
    let addr = args[0] + cpu.Y |> uint16
    let value = addr |> memRead bus
    sprintf "@ %02X = %02X" addr value
  | Absolute ->
    match op with
    | JMP | JSR -> ""
    | _ ->
      let hi = args[1] |> uint16
      let lo = args[0] |> uint16
      let addr = hi <<< 8 ||| lo |> uint16
      let value = addr |> memRead bus
      sprintf "= %02X" value
  | Absolute_X ->
      let hi = args[1] |> uint16
      let lo = args[0] |> uint16
      let addr = (hi <<< 8 ||| lo) + uint16 cpu.X
      let value = addr |> memRead bus
      sprintf "@ %04X = %02X" addr value
  | Absolute_Y ->
      let hi = args[1] |> uint16
      let lo = args[0] |> uint16
      let addr = (hi <<< 8 ||| lo) + uint16 cpu.Y
      let value = addr |> memRead bus
      sprintf "@ %04X = %02X" addr value
  | Indirect_X ->
    let bpos = args[0]
    let ptr = bpos + cpu.X
    let addr = ptr |> memRead16ZeroPage bus
    let value = addr |> memRead bus
    sprintf "@ %02X = %04X = %02X" ptr addr value
  | Indirect_Y ->
    let bpos = args[0]
    let deRefBase = bpos |> memRead16ZeroPage bus
    let deRef = deRefBase + (cpu.Y |> uint16)
    let value = deRef |> memRead bus
    sprintf "= %04X @ %04X = %02X" deRefBase deRef value
  | Indirect ->
    let hi = args[1] |> uint16
    let lo = args[0] |> uint16
    let bpos = hi <<< 8 ||| lo
    let addr = bpos |> memRead16Wrap bus // JMP のページ境界バグ
    sprintf "= %04X" addr
  | _ -> ""

let formatInstructionBytes opcode (args: byte[]) =
  Array.concat [ [| opcode |]; args ]
  |> Array.map (fun b -> b.ToString("X2"))
  |> String.concat " "

let formatCpuStatus cpu =
  sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" cpu.A cpu.X cpu.Y cpu.P cpu.SP

let getMnemonicName (x: 'T) =
  match FSharpValue.GetUnionFields(x, typeof<'T>) with
    | case, _ -> case.Name

let trace cpu bus =
  let opcode = cpu.PC |> memRead bus
  let op, mode, size, cycles = decodeOpcode opcode
  match op with
  | BRK -> "" // BRK の場合とりあえずトレースは飛ばしておく
  | _ ->
    let args = Array.init (int size - 1) (fun i -> cpu.PC + uint16(i + 1) |> memRead bus)
    let bin = formatInstructionBytes opcode args
    let mn = getMnemonicName op
    let addr = formatAddress cpu.PC mode args
    let mem = formatMemoryAccess cpu bus op mode args
    let asm = [|mn; addr; mem|] |> String.concat " "

    let pc = sprintf "%04X" cpu.PC
    let st = formatCpuStatus cpu

    sprintf "%-6s%-10s%-32s%s" pc bin asm st