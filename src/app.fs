module App

open Fable.Core.JS
open Browser.Dom
open Browser.Types
open Vide

let mutable currentState = None

let demos : list<string * string * (HTMLElement -> unit)> = 
    let inline start demo = fun host ->
        let onEvaluated _ state = currentState <- state |> Option.map (fun s -> s :> obj)
        let videMachine = prepareStart host demo onEvaluated
        videMachine.Eval()

    [
        (
            "Hello World",
            "Just a message to the world...",
            start Demos.helloWorld
        )

        (
            "Counter",
            "The famous, one-of-a kind counter.",
            start Demos.counter
        )
        
        (
            "Conditional attributes",
            "Count to 5 and you'll get a surprise!",
            start Demos.conditionalAttributes
        )

        (
            "Conditional elements (if/else)",
            "Count to 5 and you'll get another surprise!",
            start Demos.conditionalElement
        )

        (
            "List of elements",
            "Just an immutable list.",
            start Demos.simpleFor
        )

        (
            "Mutable element list",
            "Add / Remove items",
            start Demos.statelessFor
        )

        (
            "List with element state",
            "TODO",
            start Demos.statefulFor
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

document.getElementById("logState").onclick <- fun _ ->
    console.log(JSON.stringify(currentState, space = 2))
