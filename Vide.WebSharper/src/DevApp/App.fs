[<WebSharper.JavaScript>]
module DevApp.App

open WebSharper.JavaScript
open Vide
open DevApp

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
        let app = VideApp.createWebSharperWithObjState host content (fun _ _ _ -> ())
        do currentApp <- Some app
        do app.EvaluationManager.RequestEvaluation()
    let demos =
        [
            {|
                category = "Screen Recorsding"
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
                        start Demos.GettingStarted.helloWorld
                    )

                    (
                        "Counter",
                        "The famous, one-of-a kind counter.",
                        start Demos.GettingStarted.counter
                    )
        
                    (
                        "Conditional attributes",
                        "Count to 5 and you'll get a surprise!",
                        start Demos.GettingStarted.conditionalAttributes
                    )

                    (
                        "Conditional elements (multiple if)",
                        "Count to 5 and you'll get another surprise!",
                        start Demos.GettingStarted.conditionalIfs
                    )

                    (
                        "Conditional elements (if/else)",
                        "TODO: This must be documented!",
                        start Demos.GettingStarted.conditionalIfElse
                    )

                    (
                        "List of elements",
                        "Just an immutable list.",
                        start Demos.GettingStarted.simpleFor
                    )

                    (
                        "Mutable element list",
                        "Add / Remove items",
                        start Demos.GettingStarted.statelessFor
                    )

                    (
                        "List with element state",
                        "TODO",
                        start Demos.GettingStarted.statefulFor
                    )
                ]
            |}

            {|
                category = "Components"
                elements = [
                    (
                        "Component emitting a value",
                        "TODO",
                        start (Demos.Components.visualComponentReturningValues)
                    )
                ]
            |}

            {|
                category = "Async"
                elements = [
                    (
                        "Async (hello world)",
                        "TODO",
                        start Demos.Async.asyncHelloWorld
                    )

                    (
                        "Async (inside elements)",
                        "TODO",
                        start Demos.Async.asyncInsideHtmlElements
                    )

                    (
                        "Async (with return values)",
                        "TODO",
                        start Demos.Async.asyncWithSubsequentResults
                    )
                ]
            |}

            {|
                category = "Input"
                elements = [
                    (
                        "Retrieving text values",
                        "TODO",
                        start Demos.Input.textInputReturnsValue
                    )
                    (
                        "Retrieving text values (as state)",
                        "TODO",
                        start Demos.Input.textInputReturnsValue
                    )
                    (
                        "Component with input",
                        "TODO",
                        start Demos.Input.textInputComponent
                    )
                ]
            |}

            {|
                category = "Advanced"
                elements = [
                    (
                        "Direct access to HTMLElement (on init and eval)",
                        "TODO",
                        start Demos.Advanced.directAccessToHtmlElementViaInitAndEval
                    )
                    //(
                    //    "Direct access to HTMLElement (via computation)",
                    //    "TODO",
                    //    start Demos.Advanced.directAccessToHtmlElementViaComputation
                    //)
                ]
            |}

            {|
                category = "Apps"
                elements = [
                    (
                        "Todo List",
                        "TODO",
                        start Demos.TodoList.view
                    )
                ]
            |}
        ]
    demos

let start () =
    let menu = JS.Document.GetElementById("menu")
    let demoHost = JS.Document.GetElementById("demo")
    for x in demos do
        do
            let cat = JS.Document.CreateElement("h4")
            cat.TextContent <- x.category
            menu.AppendChild(cat) |> ignore
        for title,desc,runDemo in x.elements do
            let btn = As<HTMLButtonElement>(JS.Document.CreateElement("button"))
            btn.InnerText <- title
            btn.Onclick <- fun _ ->
                //do Console.clear()
                let innerDemoHostId = "innerDemoHost"
                demoHost.InnerHTML <-
                    $"""
                    <h2>{title}</h2> 
                    <blockquote>{desc}</blockquote>
                    <div id={innerDemoHostId}></div>
                    """
                let innerDemoHost = demoHost.QuerySelector($"#{innerDemoHostId}")
                runDemo innerDemoHost
            menu.AppendChild(btn) |> ignore

    As<HTMLButtonElement>(JS.Document.GetElementById("evaluate")).Onclick <- fun _ ->
        currentApp |> Option.iter (fun app -> app.EvaluationManager.RequestEvaluation())

    As<HTMLButtonElement>(JS.Document.GetElementById("logState")).Onclick <- fun _ ->
        currentApp |> Option.iter (fun app -> Console.Log(app))
