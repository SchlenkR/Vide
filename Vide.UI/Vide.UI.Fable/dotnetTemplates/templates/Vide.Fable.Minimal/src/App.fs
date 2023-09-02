module App

open Fable.Core.JsInterop
open Browser
open Vide

importSideEffects("./App.scss")

let host = document.getElementById("app")
let app = VideApp.ForHost(host).CreateAndStartWithUntypedState(Components.Demo.view)

()
