module App

open Browser.Dom
open Browser.Types
open Vide

let menu = document.getElementById("menu")
let demoHost = document.getElementById("demo")
for desc,runDemo in Demos.demos do
    let btn = document.createElement("button") :?> HTMLButtonElement
    btn.innerText <- desc
    btn.addEventListener("click", fun evt ->
        demoHost.innerHTML <- ""
        runDemo demoHost
    )
    menu.appendChild(btn) |> ignore
