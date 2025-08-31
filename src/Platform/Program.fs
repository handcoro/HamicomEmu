open System
open Tests
open Expecto
open HamicomEmu.Platform

[<EntryPoint>]
let main argv =
    if argv |> Array.exists ((=) "--test") then
        // --test を除いた引数だけ Expecto に渡す
        let filteredArgs = argv |> Array.filter (fun a -> a <> "--test")
        runTestsWithArgs defaultConfig filteredArgs Tests.tests
    else
        let traceFn =
            if argv |> Array.exists ((=) "--trace") then
                HamicomEmu.Trace.traceAndPrint
            else
                fun _ -> ()

        match argv |> Array.tryHead with
        | Some path ->
            GameHost.run path traceFn
            0
        | None ->
            GameHost.run GameHost.defaultCart traceFn
            0
