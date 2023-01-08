module Demos

open Vide
open type Vide.Html

// TODO: Move those 2
type BuilderOperations = | Clear
type VideBuilder with
    member inline _.Yield
        (op: BuilderOperations) 
        : Vide<unit,unit,FableContext>
        =
        Vide <| fun s ctx ->
            match op with
            | Clear -> ctx.Parent.textContent <- ""
            (),None

    // TODO: returning values

let asyncHelloWorld =
    let waitTimeInMs = 1000
    let loadingMessage phase = $"loading phase %d{phase}... please wait {float waitTimeInMs / 1000.0} seconds"
    let finishedMessage phase res = $"Phase %d{phase} finished with result = {res}"

    let myAsyncComponent = vide {
        return 0

        p { loadingMessage 1 }
        let! res1 = async {
            do! Async.Sleep waitTimeInMs
            return 42
        }
        p { finishedMessage 1 res1 }
        return 1

        p { loadingMessage 2 }
        let! res2 = async {
            do! Async.Sleep waitTimeInMs
            return 187
        }
        p { finishedMessage 2 res2 }
        return 2
        
        p { loadingMessage 3 }
        do! Async.Sleep waitTimeInMs

        // TODO: the returns don't work (look for Unckecked.default...)

        // TODO: demo for async + clear
        //Clear
        p { "--- END ---" }
        return 3
    }

    vide {
        // TODO: Interesting use case for component result
        // handling and ordering / evaluation

        let! currRes = Mutable.ofValue 0
        div {
            p { $"Current component result: {currRes.Value}" }
            p { "------------" }
        }
        
        let! componentResult = myAsyncComponent
        currRes := componentResult
    }

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        let! count = Mutable.ofValue 0

        div { $"Count = {count.Value}" }
        button.onclick(fun _ -> count -= 1) { "dec" }
        button.onclick(fun _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        "TODO: That doesn'a work right now"
        div.className("the-message") {
            nothing
            //span.hidden(count.Value <> 5) {
            //    "You have the right to defend yourself!"
            //}
        }
    }

let conditionalIfs =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        // TODO. Docu: The state can get lost because they are not compatible

        if count.Value = 5 || count.Value = 6 then
            let! valueString = preserve "Hello String"
            div.className("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        // TODO:
        // else zero: instead of "zero", it could be controlled if component state
        // in the corresponding "if" will be preserved or cleared!
        else zero

        // this must compile
        nothing

        if count.Value <> 5 then
            let! valueInt = preserve 42
            p { $"not yet... with int value {valueInt}" }
        else zero
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let conditionalIfElse =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        "if-else cannot work like that"

        ////// TODO: That should not be used at all? And: That this seems to work
        ////// is only an edge case, because state has same type
        ////if count.Value = 5 then
        ////    div.className("the-message") { 
        ////        $"You have the right to defend yourself!" 
        ////    }
        ////else
        ////    p { $"not yet..." }
    }

let simpleFor =
    vide {
        for x in 0..5 do
            div.className("card") { $"I'm element no. {x}" }
    }

let statelessFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items :=  []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items := []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Mutable.ofValue 0
                button.onclick(fun _ -> count -= 1) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ -> count += 1) { "inc" }
        }
    }

// TODO: If that's not a function, we will have som val restr issues. Examine those!
let visualComponentReturningValues () =
    let visualCounter =
        vide {
            let! count = Mutable.ofValue 0

            button.onclick(fun _ -> count -= 1) { "dec" }
            button.onclick(fun _ -> count += 1) { "inc" }

            return count.Value
        }

    vide {
        let! count = visualCounter
        p { $"COUNT = {count}"}
    }

let directAccessToHtmlElement =
    vide {
        div.OnEval(fun x -> x.className <- "bam")  {
            "I'm the OnEval div"
        }

        div.OnInit (fun x -> x.className <- "bam2") {
            "I'm the OnInit div"
        }
    }
