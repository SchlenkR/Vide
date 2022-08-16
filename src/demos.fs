module Demos

open Vide
open type Html

let helloWorld =
    vide {
        text "Hello World"
    }

let counter =
    vide {
        let! count = Mutable.ofValue 0

        div {
            text $"Count = {count.Value}"
        }

        button .onclick(fun _ -> count -= 1) { "dec" }
        button .onclick(fun _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button .onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        div .class'("the-message") {
            span .hidden(count.Value <> 5) {
                "You have the right to defend yourself!"
            }
        }
    }

let conditionalElement =
    vide {
        let! count = Mutable.ofValue 0

        button .onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        if count.Value = 5 then
            div .class'("the-message") { "You have the right to defend yourself!" }
        else
            p { "not yet ..." }
    }

let simpleFor =
    vide {
        for x in 0..5 do
            div .class'("card") { $"I'm element no. {x}" }
    }

let nextNum() = System.Random().Next(10000)

let statelessFor =
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items :=  []

        button .onclick(add1) { "Add One" }
        button .onclick(add100) { "Add 100" }
        button .onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div .class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button .onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items := []

        button .onclick(add1) { "Add One" }
        button .onclick(add100) { "Add 100" }
        button .onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div .class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button .onclick(removeMe) { $"Remove {x}" }

                let! count = Mutable.ofValue 0
                button .onclick(fun _ -> count -= 1) { "dec" }
                text $"{count.Value}  "
                button .onclick(fun _ -> count += 1) { "inc" }
        }
    }
