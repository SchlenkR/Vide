module Counter

open Fable.Core.JsInterop
open Browser
open Vide
open type Html

importSideEffects("./styles/Counter.css")

let internal counterView =
    vide {
        let! count = Vide.ofMutable 0

        p { $"count = {count.Value}" }

        p {
            button.onclick (fun _ -> count -= 1) { "dec" }
            button.onclick (fun _ -> count += 1) { "inc" }
        }
    }

let view =
    vide {
        div.class' ("main") {
            p { img.src("./src/assets/logo.png").width ("150px") }
            counterView
        }
    }
