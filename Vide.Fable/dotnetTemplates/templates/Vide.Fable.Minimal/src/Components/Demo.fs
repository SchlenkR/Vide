module Components.Demo

open Fable.Core.JsInterop
open Vide
open type Html

importSideEffects("./Demo.scss")

let view =
    vide {
        div.class' ("main-view") {
            p { img.src("./src/Components/logo.png").width("150px") }
            hr
            "The whole Vide world"
        }
    }
