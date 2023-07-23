module DemoApp

open Browser.Dom
open Fable.Core.JS
open Vide

let content = DemoApp.TodoList.view
let host = document.getElementById("host")
let app = VideApp.createAndStart(host, content) :> IApp

document.getElementById("evaluate").onclick <- fun _ ->
    app.RequestEvaluation()

document.getElementById("logState").onclick <- fun _ ->
    console.log(app)
