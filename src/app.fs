module App

open Browser.Dom
open Browser.Types
open Vide

let demos : list<string * string * (HTMLElement -> unit)> = [
    (
        "Hello World",
        "Just a message to the world...",
        fun host -> Demos.helloWorld |> start host
    )
    (
        "Counter",
        "The famous, one-of-a kind counter.",
        fun host -> Demos.counter |> start host    
    )
    (
        "Conditional Attributes",
        "Count to 5 and you'll get a surprise!",
        fun host -> Demos.conditionalAttributes |> start host
    )
]

let menu = document.getElementById("menu")
let demoHost = document.getElementById("demo")
for title,desc,runDemo in demos do
    let btn = document.createElement("button") :?> HTMLButtonElement
    btn.innerText <- title
    btn.addEventListener("click", fun evt ->
        let innerDemoHostId = "innerDemoHost"
        demoHost.innerHTML <-
            $"""
            <h2>{title}</h2>
            <blockquote>{desc}</blockquote>
            <div id={innerDemoHostId}></div>
            """
        let innerDemoHost = demoHost.querySelector($"#{innerDemoHostId}") :?> HTMLElement
        runDemo innerDemoHost
    )
    menu.appendChild(btn) |> ignore
