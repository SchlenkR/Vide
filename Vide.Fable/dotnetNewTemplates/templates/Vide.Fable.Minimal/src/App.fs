module App

open Fable.Core.JsInterop
open Browser
open Vide

importSideEffects("./App.scss")

let host = document.getElementById("app")
let app = VideApp.Fable.createAndStart host Components.Demo.view

()
