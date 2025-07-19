namespace HamicomEmu.Mapper

module Common =

    // TODO: ミラーリングの型の配置場所を考える
    type Mirroring =
        | Horizontal
        | Vertical
        | FourScreen

    let getOffset calcBank bankSize baseAddr = calcBank * bankSize - baseAddr
