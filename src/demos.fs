module Demos

open Vide
open type Vide.Html

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
        "TODO: That doesn't work right now"
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

        if count.Value = 5 || count.Value = 6 then
            let! valueString = preserve "Hello String"
            div.className("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        if count.Value <> 5 then
            let! valueInt = preserve 42
            p { $"not yet... with int value {valueInt}" }
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

//let directAccessToHtmlElement1 =
//    vide {
//        let! countDivHtmlElement = 
//            div { "Hello" }
        
//        // we need to yield at least nothing
//        nothing
//    }

//let directAccessToHtmlElement2 =
//    vide {
//        let! count = Mutable.ofValue 0

//        // TODO: Control what to emit
//        // TODO: this looks strange: yielding + binding. Provide a "onRender" mechanism o.ä.
//        let! countDivHtmlElement = 
//            div { $"Count = {count.Value}" }
//        do countDivHtmlElement.className <- "bam"

//        // TODO: should also work with builder (without converting to Vide)?
//        // TODO: div {()} gives ValueRestriction (generally {()} or () is problematic)
//        //let! emptyDivElement = div { nothing }
//        let! emptyDivElement = div
//        do emptyDivElement.className <- "bam2"

//        p {
//            div
//        }
//        span
//    }

//let asyncSample =
//    vide {
//        button.onclick(fun _ -> count += 1) { "wair 5 seconds, then " }

//        let! items = Mutable.ofValue []
//        let add1 _ = items := items.Value @ [nextNum()]
//        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
//        let removeAll _ = items := []

//        button.onclick(add1) { "Add One" }
//        button.onclick(add100) { "Add 100" }
//        button.onclick(removeAll) { "Remove All" }
        
//        for x in items.Value do
//            div.className("card") {
//                let removeMe _ = items := items.Value |> List.except [x]
//                button.onclick(removeMe) { $"Remove {x}" }

//                let! count = Mutable.ofValue 0
//                button.onclick(fun _ -> count -= 1) { "dec" }
//                $"{count.Value}  "
//                button.onclick(fun _ -> count += 1) { "inc" }
//        }
//    }
