namespace HamicomEmu.Platform

module Gui =
    open Myra
    open Myra.Graphics2D.UI
    open Microsoft.Xna.Framework

    type GuiState = { desktop: Desktop option }
    let mutable state : GuiState = { desktop = None }

    let init (game: Game) =
        // Myra の初期化や Desktop 構築はここで行う（後で実装）
        let desktop = new Desktop()
        state <- { desktop = Some desktop }

    let update (gameTime: GameTime) =
        match state.desktop with
        | Some d -> () // 必要に応じて更新処理
        | None -> ()

    let draw () =
        match state.desktop with
        | Some d -> () // 描画処理（Myra のレンダラ呼び出し等）
        | None -> ()

