module Trace

open Instructions
open Cpu
open Bus

open Microsoft.FSharp.Reflection

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
    let pc' = pc + 2us |> int
    sprintf "$%04X" (args[0] |> sbyte |> int |> (+) pc' |> uint16)
  | Implied ->
    ""

let formatMemoryAccess cpu bus op mode (args: byte[]) =
  match mode with
  | ZeroPage ->
    let addr = args[0] |> uint16
    let value, _ = memRead addr bus
    sprintf "= %02X" value
  | ZeroPage_X ->
    let addr = args[0] + cpu.X |> uint16
    let value, _ = memRead addr bus
    sprintf "@ %02X = %02X" addr value
  | ZeroPage_Y ->
    let addr = args[0] + cpu.Y |> uint16
    let value, _ = memRead addr bus
    sprintf "@ %02X = %02X" addr value
  | Absolute ->
    match op with
    | JMP | JSR -> ""
    | _ ->
      let hi = args[1] |> uint16
      let lo = args[0] |> uint16
      let addr = hi <<< 8 ||| lo
      let value, _ = memRead addr bus
      sprintf "= %02X" value
  | Absolute_X ->
    let hi = args[1] |> uint16
    let lo = args[0] |> uint16
    let addr = (hi <<< 8 ||| lo) + uint16 cpu.X
    let value, _ = memRead addr bus
    sprintf "@ %04X = %02X" addr value
  | Absolute_Y ->
    let hi = args[1] |> uint16
    let lo = args[0] |> uint16
    let addr = (hi <<< 8 ||| lo) + uint16 cpu.Y
    let value, _ = memRead addr bus
    sprintf "@ %04X = %02X" addr value
  | Indirect_X ->
    let bpos = args[0]
    let ptr = bpos + cpu.X
    let addr, _ = memRead16ZeroPage ptr bus
    let value, _ = memRead addr bus
    sprintf "@ %02X = %04X = %02X" ptr addr value
  | Indirect_Y ->
    let bpos = args[0]
    let deRefBase, _ = memRead16ZeroPage bpos bus
    let deRef = deRefBase + (cpu.Y |> uint16)
    let value, _ = memRead deRef bus
    sprintf "= %04X @ %04X = %02X" deRefBase deRef value
  | Indirect ->
    let hi = args[1] |> uint16
    let lo = args[0] |> uint16
    let bpos = hi <<< 8 ||| lo
    let addr, _ = memRead16Wrap bpos bus // JMP のページ境界バグ
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
    | case, _ ->
      let name = case.Name
      if name.EndsWith "_" then "*" + name.TrimEnd '_' else " " + name

let readArgs bus start count =
  let folder (acc, b) i =
    let data, b' = memRead (start + uint16 i) b
    (data :: acc, b')
  let result, finalBus = List.fold folder ([], bus) [1 .. count]
  (List.rev result |> List.toArray), finalBus

let trace cpu bus =
  let opcode, _ = memRead cpu.PC bus
  let op, mode, size, cycles = decodeOpcode opcode
  match op with
  | BRK -> "" // BRK の場合とりあえずトレースは飛ばしておく
  | _ ->
    let args, bus' = readArgs bus cpu.PC (int size - 1)
    let bin = formatInstructionBytes opcode args
    let mn = getMnemonicName op
    let addr = formatAddress cpu.PC mode args
    let mem = formatMemoryAccess cpu bus op mode args
    let asm = [|mn; addr; mem|] |> String.concat " "

    let pc = sprintf "%04X" cpu.PC
    let st = formatCpuStatus cpu

    sprintf "%-6s%-9s%-33s%s" pc bin asm st