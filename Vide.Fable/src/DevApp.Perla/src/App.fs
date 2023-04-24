module App

open Browser.Dom
open Browser.Types
open Fable.Core.JS
open Vide

Fable.Core.JsInterop.importSideEffects "../assets/index.css?js"

do Debug.enabledDebugChannels <- 
    [
        //0
        1
        10
    ]

let mutable currentApp = None

let demos = 
    let inline start content host =
        let content = content |> Vide.map ignore
        let app = VideApp.Fable.createWithUntypedState host content
        do currentApp <- Some app
        do app.EvaluationManager.RequestEvaluation()
    let demos =
        [
            {|
                category = "Screen Recording"
                elements = [
                    (
                        "Clear screen (recording)",
                        "Used for recording GIFs",
                        start <| vide { () }
                    )
                ]
            |}

            {|
                category = "Getting Started"
                elements = [
                    (
                        "Hello World",
                        "Just a message to the world...",
                        start UseCases.GettingStarted.helloWorld
                    )

                    (
                        "Counter",
                        "The famous, one-of-a kind counter.",
                        start UseCases.GettingStarted.counter
                    )
        
                    (
                        "Conditional attributes",
                        "Count to 5 and you'll get a surprise!",
                        start UseCases.GettingStarted.conditionalAttributes
                    )
                ]
            |}

            {|
                category = "If-Else"
                elements = [
                    (
                        "Conditional (if - else forget)",
                        "Count to 5 and you'll get another surprise!",
                        start UseCases.IfElse.ifElseWithForget
                    )

                    (
                        "Conditional (if - else preserve)",
                        "TODO: This must be documented!",
                        start UseCases.IfElse.ifElseWithPreserve
                    )
                ]
            |}

            {|
                category = "For (Loops)"
                elements = [
                    (
                        "List of elements",
                        "Just an immutable list.",
                        start UseCases.For.simpleFor
                    )

                    (
                        "Mutable element list",
                        "Add / Remove items",
                        start UseCases.For.statelessFor
                    )

                    (
                        "List with element state",
                        "TODO",
                        start UseCases.For.statefulFor
                    )
                ]
            |}

            {|
                category = "Components"
                elements = [
                    (
                        "Component emitting a value",
                        "TODO",
                        start UseCases.Components.visualComponentReturningValues
                    )
                ]
            |}

            {|
                category = "Async"
                elements = [
                    (
                        "Async (hello world)",
                        "TODO",
                        start UseCases.Async.asyncHelloWorld
                    )

                    (
                        "Async (inside elements)",
                        "TODO",
                        start UseCases.Async.asyncInsideHtmlElements
                    )

                    (
                        "Async (with return values)",
                        "TODO",
                        start UseCases.Async.asyncWithSubsequentResults
                    )
                ]
            |}

            {|
                category = "Input"
                elements = [
                    (
                        "Retrieving text values",
                        "TODO",
                        start UseCases.Input.textInputReturnsValue
                    )
                    (
                        "Retrieving text values (as state)",
                        "TODO",
                        start UseCases.Input.textInputReturnsValue
                    )
                    (
                        "Component with input",
                        "TODO",
                        start UseCases.Input.textInputComponent
                    )
                ]
            |}

            {|
                category = "Advanced"
                elements = [
                    (
                        "Direct access to HTMLElement (on init and eval)",
                        "TODO",
                        start UseCases.Advanced.directAccessToHtmlElementViaInitAndEval
                    )
                    //(
                    //    "Direct access to HTMLElement (via computation)",
                    //    "TODO",
                    //    start UseCases.Advanced.directAccessToHtmlElementViaComputation
                    //)
                ]
            |}

            {|
                category = "Apps"
                elements = [
                    (
                        "Todo List",
                        "TODO",
                        start UseCases.TodoList.view
                    )
                ]
            |}
        ]
    demos

let menu = document.getElementById("menu")
let demoHost = document.getElementById("demo")
for x in demos do
    do
        let cat = document.createElement("h4")
        cat.textContent <- x.category
        menu.appendChild(cat) |> ignore
    for title,desc,runDemo in x.elements do
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
    currentApp |> Option.iter (fun app -> app.EvaluationManager.RequestEvaluation())

document.getElementById("logState").onclick <- fun _ ->
    currentApp |> Option.iter (fun app -> console.log(app))
