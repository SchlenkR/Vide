module App

open Fable.Core.JsInterop
open Browser
open Vide
open type Html

importSideEffects("./styles/App.css")

let host = document.getElementById("app")
let app = VideApp.Fable.createWithUntypedState host Counter.view (fun _ _ _ -> ())
do app.EvaluationManager.RequestEvaluation()
