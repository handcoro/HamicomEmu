namespace HamicomEmu.Mapper

module Common =

    let getOffset calcBank bankSize baseAddr = calcBank * bankSize - baseAddr
