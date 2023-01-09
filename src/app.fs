module App

open Browser.Dom
open Browser.Types
open Fable.Core.JS
open Vide

do Debug.enabledDebugChannels <- 
    [
        //0
        1
        10
    ]

let mutable currentApp = None

let demos : list<string * string * (HTMLElement -> unit)> = 
    let inline start content host =
        let content = content |> map ignore
        let app = App.createFableWithObjState host content (fun _ _ -> ())
        do currentApp <- Some app
        do app.RequestEvaluation()
    let demos =
        [
            //(
            //    "Async (trigger)",
            //    "TODO",
            //    start Demos.asyncTrigger
            //)

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
                "Conditional elements (multiple if)",
                "Count to 5 and you'll get another surprise!",
                start Demos.conditionalIfs
            )

            (
                "Conditional elements (if/else)",
                "TODO: This must be documented!",
                start Demos.conditionalIfElse
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

            (
                "Component emitting a value",
                "TODO",
                start (Demos.visualComponentReturningValues ())
            )

            (
                "Direct access to HTMLElement (on init and eval)",
                "TODO",
                start Demos.directAccessToHtmlElement
            )

            (
                "Async ('Hello World')",
                "TODO",
                start Demos.asyncHelloWorld
            )

            (
                "Async (inside elements)",
                "TODO",
                start Demos.asyncInsideHtmlElements
            )

            (
                "Async (with return values)",
                "TODO",
                start Demos.asyncWithSubsequentResults
            )
        ]
    demos

let menu = document.getElementById("menu")
let demoHost = document.getElementById("demo")
for title,desc,runDemo in demos do
    let btn = document.createElement("button") :?> HTMLButtonElement
    btn.innerText <- title
    btn.onclick <- fun _ ->
        do console.clear()
        let innerDemoHostId = "innerDemoHost"
        demoHost.innerHTML <-
            $"""
            <h2>{title}</h2> 
            <blockquote>{desc}</blockquote>
            <div id={innerDemoHostId}></div>
            """
        let innerDemoHost = demoHost.querySelector($"#{innerDemoHostId}") :?> HTMLElement
        runDemo innerDemoHost
    menu.appendChild(btn) |> ignore

document.getElementById("evaluate").onclick <- fun _ ->
    currentApp |> Option.iter (fun app -> app.RequestEvaluation())

document.getElementById("logState").onclick <- fun _ ->
    currentApp |> Option.iter (fun app -> console.log(app))
