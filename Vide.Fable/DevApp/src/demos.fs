module Demos

open Vide
open type Vide.Html

// TODO: demo for async + clear
//Clear

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
        let! currRes = Mutable.ofValue 0
        div.className("async-box") {
            $"Current component result: {currRes.Value}"
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
        button.onclick(fun _ _ -> count -= 1) { "dec" }
        button.onclick(fun _ _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        "TODO: That doesn'a work right now"
        div.className("the-message") {
            ()
            //span.hidden(count.Value <> 5) {
            //    "You have the right to defend yourself!"
            //}
        }
    }

let conditionalIfs =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ _ -> count += 1) {
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
        ()

        if count.Value <> 5 then
            let! valueInt = preserve 42
            p { $"not yet... with int value {valueInt}" }
        else zero
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let conditionalIfElse =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ _ -> count += 1) {
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
        let add1 _ _ = items := items.Value @ [nextNum()]
        let add100 _ _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ _ = items :=  []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Mutable.ofValue []
        let add1 _ _ = items := items.Value @ [nextNum()]
        let add100 _ _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ _ = items := []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Mutable.ofValue 0
                button.onclick(fun _ _ -> count -= 1) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ _ -> count += 1) { "inc" }
        }
    }

// TODO: If that's not a function, we will have som val restr issues. Examine those!
let visualComponentReturningValues () =
    let visualCounter =
        vide {
            let! count = Mutable.ofValue 0

            button.onclick(fun _ _ -> count -= 1) { "dec" }
            button.onclick(fun _ _ -> count += 1) { "inc" }

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
