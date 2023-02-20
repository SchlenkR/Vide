module DevApp.CompilerOnlyDemos

open System
open Browser.Types
open Vide
open type Vide.Html

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        div { $"Hello" }
        p { "xxx" }
    }

let counter =
    vide {
        let! count = Vide.ofMutable 0

        div { $"Count = {count.Value}" }
        button.onclick(fun _ -> count -= 1) { "dec" }
        button.onclick(fun _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        div.class'("the-message") {
            span.hidden(count.Value <> 5) {
                "You have the right to defend yourself!"
            }
        }
    }

let conditionalIfs =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        // TODO. Docu: The state can get lost because they are not compatible

        if count.Value = 5 || count.Value = 6 then
            let! valueString = Vide.preserveValue "Hello String"
            div.class'("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        // TODO:
        // else zero: instead of "zero", it could be controlled if component state
        // in the corresponding "if" will be preserved or cleared!
        else Vide.zero

        // this must compile
        ()

        if count.Value <> 5 then
            let! valueInt = Vide.preserveValue 42
            p { $"not yet... with int value {valueInt}" }
        else Vide.zero
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let conditionalIfElse =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        "if-else cannot work like that"

        ////// TODO: That should not be used at all? And: That this seems to work
        ////// is only an edge case, because state has same type
        ////if count.Value = 5 then
        ////    div.class'("the-message") { 
        ////        $"You have the right to defend yourself!" 
        ////    }
        ////else
        ////    p { $"not yet..." }
    }

let simpleFor =
    vide {
        for x in 0..5 do
            div.class'("card") { $"I'm element no. {x}" }
    }

let statelessFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Vide.ofMutable []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items :=  []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Vide.ofMutable []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items := []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Vide.ofMutable 0
                button.onclick(fun _ -> count -= 1) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ -> count += 1) { "inc" }
        }
    }

(*
module Input =
    let textInputReturnsValue = 
        vide {
            // TODO: Docu - wie ändert man das PropertyChangedTrigger-Verhalten
            let! enteredValue = input.type'("text").oninput()
            div {
                $"You say: {enteredValue.TextValue}"
            }
        }
    
    let textInputEvent = 
        vide {
            let! enteredText = Vide.ofMutable ""

            div {
                $"You say: %s{enteredText.Value}"
            }

            // TODO: Docu - Default-Verhalten von input
            input.type'("text").oninput(fun e -> enteredText.Value <- e.node.value)
        }
    
    let textInputComponent = 
        vide {
            let! enteredText = div {
                let! enteredValue = input.type'("text").oninput()
                return enteredValue.TextValue
            }

            div {
                $"You say: {enteredText}"
            }
        }

module Components =

    // TODO: If that's not a function, we will have som val restr issues. Examine those!
    let visualComponentReturningValues =
        let visualCounter =
            vide {
                let! count = Vide.ofMutable 0

                button.onclick(fun _ -> count -= 1) { "dec" }
                button.onclick(fun _ -> count += 1) { "inc" }

                return count.Value
            }

        vide {
            let! count = visualCounter
            p { $"COUNT = {count}"}
        }


module Async =
    let asyncHelloWorld =

        vide {
            p { "loading 1st number ..." }
            let! res1 = async {
                do! Async.Sleep 2000
                return 42
            }
            p { $"1st number is: {res1}" }
            hr

            p { "loading 2nd number..." }
            let! res2 = async {
                do! Async.Sleep 2000
                return 187
            }
            p { $"2nd number is: {res2}" }
            hr
        
            p { "waiting again for a whie..." }
            do! Async.Sleep 2000
            p { "Done :)" }
        }

    let asyncInsideHtmlElements =

        // You can also put asyncs inside of HTML elements :)

        vide {
            p { 
                "loading 1st number ..."
                let! res1 = async {
                    do! Async.Sleep 2000
                    return 42
                }
                $"{res1}"
            }
            hr

            p { 
                "loading 2nd number ..." 
                let! res2 = async {
                    do! Async.Sleep 2000
                    return 187
                }
                $"{res2}"
            }
            hr
        
            p {
                "waiting (in parallel) for a whie..."
                do! Async.Sleep 2000
                "Done :)"
            }
        }

    // TODO: Order of return / yield HTMLElement shouldn't matter

    let asyncWithSubsequentResults =

        let myAsyncComponent =
            // TODO: Docu: Before(!) every awaited value must be a return.
            vide {
                p { "loading 1st number ..." }
                return 0

                let! res1 = async {
                    do! Async.Sleep 2000
                    return 42
                }
                p { $"1st number is: {res1}" }
                hr

                p { "loading 2nd number ..." }
                return res1
                
                let! res2 = async {
                    do! Async.Sleep 2000
                    return 187
                }
                p { $"2nd number is: {res2}" }
                hr
            
                p { "waiting again for a whie..." }
                return res2
                
                do! Async.Sleep 2000
                p { "Done :)" }

                return 999
            }

        vide {
            let! currRes = Vide.ofMutable 0
            div.class'("async-box") {
                $"Current component result: {currRes.Value}"
            }
        
            let! componentResult = myAsyncComponent
            currRes := componentResult
        }


module Advanced =
    let directAccessToHtmlElementViaInitAndEval =
        vide {
            div.OnEval(fun x -> x.node.className <- "bam")  {
                "I'm the OnEval div"
            }

            div.OnInit(fun x -> x.node.className <- "bam2") {
                "I'm the OnInit div"
            }
        }

    //let directAccessToHtmlElementViaComputation =
    //    vide {
    //        let x = div {
    //            // TODO: This (specifying gen arg + internal cast) is not what I want...

    //            let! htmlDivElement = Vide.node<HTMLDivElement>
    //            $"...we can now access HTML element: {htmlDivElement.nodeName}"
    //        }
    //        x
    //    }

    let shouldCompile1 =
        // Both void and content builders should be able to yield
        // from inside of a content workflow
        vide {
            div {
                hr
                p
            }
        }
    
    let shouldCompile2 =
        // Both void and content builders should be able to yield
        // from inside of a vide workflow
        vide {
            hr
            p
        }

    // TODO: returning a node from inside { .. } to enable "let! node = ..."


module TodoList =
    // Document:
    // - here: mutable model (doesn't have to be that way)
    // - "Vide.ofMutable": This also doesn't have to be that way,
    //   but it's an ideomatic building block of Vide (in depth: if not,
    //   emitting control statements like in LocSta would be necessary).

    type TodoItem = { name: string; mutable isDone: bool }
    type TodoList = { items: TodoItem list }
    
    let view = vide {
        let! todoList = Vide.ofMutable { items = [] }
        
        h1.class'("title") { "TODO List" }
        div {
            let! itemName = Vide.ofMutable ""
    
            p {
                let addItem () =
                    let newItem = { name = itemName.Value; isDone = false }
                    do
                        todoList.Value <- { todoList.Value with items = newItem :: todoList.Value.items }
                        itemName.Reset()
                
                input
                    .type'("text")
                    .value(itemName.Value)
                    .oninput(fun x -> itemName.Value <- x.node.value)
                
                button
                    .disabled(String.IsNullOrWhiteSpace(itemName.Value))
                    .onclick(fun _ -> addItem()) { 
                        "Add Item" 
                    }
            }
        }
        div {
            for item in todoList.Value.items do
                div.class'("flex-row") {
                    p { item.name }
                    input
                        .type'("checkbox")
                        .checked'(item.isDone)
                        .oninput(fun x -> item.isDone <- x.node.``checked``)
                }
        }
    }
    

//module Playground =

//    let bindTest =
//        vide {
//            div.OnEval(fun x _ -> x.className <- "bam")  {
//                "I'm the OnEval div"
//            }

//            div.OnInit(fun x _ -> x.className <- "bam2") {
//                "I'm the OnInit div"
//            }
//        }

*)