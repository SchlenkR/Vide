module Components.Demo

open Fable.Core
open Fable.Core.JsInterop
open Browser
open Vide
open type Html

importSideEffects("./Demo.scss")

let view =
    vide {
        div.class' ("main-view") {
            p { img.src("./src/components/logo.png").width("150px") }
            "The whole vide world"
        }
    }
