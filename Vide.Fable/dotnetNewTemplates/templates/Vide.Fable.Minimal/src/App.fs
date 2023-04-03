module App

open Browser
open Vide
open type Html

// The famous 'counter' demo!
let counterView =
    vide {
        let! count = Vide.ofMutable 0

        p { $"count = {count.Value}" }

        p {
            button.onclick (fun _ -> count -= 1) { "dec" }
            button.onclick (fun _ -> count += 1) { "inc" }
        }
    }

let appView =
    vide {
        div.class' ("main") {
            p { img.src("./src/assets/logo.png").width ("150px") }
            counterView
        }
    }

// Bootstrap the app!
do
    let host = document.getElementById("app")
    let app = VideApp.Fable.createWithUntypedState host appView (fun _ _ _ -> ())
    do app.EvaluationManager.RequestEvaluation()
