namespace HamicomEmu.Mapper

module Common =

    // TODO: ミラーリングの型の配置場所を考える
    type Mirroring =
        | Horizontal
        | Vertical
        | FourScreen

    let inline getOffset bankSelect bankSize baseAddr = bankSelect * bankSize - baseAddr

    let vramSize = 1024 * 2 // 2 KiB