module Demos

open Browser.Dom
open Browser.Types
open Vide

open type Vide.Fable.Html

let helloWorld =
    vide {
        text "Hello World"
    }

let counter =
    vide {
        let! count = Mutable.ofValue 0
        let incDec i _ = count.Value <- count.Value + i

        div {
            text $"Count = {count.Value}"
        }

        button .onclick(incDec -1) { "dec" }
        button .onclick(incDec 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button .onclick(fun _ -> count.Value <- count.Value + 1) {
            $"Hit me! Count = {count.Value}"
        }
        div .class'("the-message") {
            span .hidden(count.Value <> 5) {
                "You have the right to defend yourself!"
            }
        }
    }

let lists =
    vide {
        for x in 0..5 do
            div .class'("card") { $"I'm element no. {x}" }
    }

let nextNum() = System.Random().Next(10000)

let mutableLists =
    vide {
        let! currentItems = Mutable.ofValue []
        let setItems items = currentItems.Value <- items

        button .onclick(fun _ -> currentItems.Value @ [nextNum()] |> setItems) { "Add One" }
        button .onclick(fun _ -> currentItems.Value @ [ for _ in 0..100 do nextNum() ] |> setItems) { "Add 100" }
        button .onclick(fun _ -> setItems []) { "Remove All" }
        
        for x in currentItems.Value do
            div .class'("card") {
                button.onclick(fun _ -> currentItems.Value <- currentItems.Value |> List.except [x]) { $"Remove {x}" }
        }
    }

// let heterogeneousLists =
//     vide {
//         let! aItems = Mutable.ofValue [ nextNum() ]
//         let addAItem item = aItems.Value <- aItems.Value @ [item]
//         let removeAItem item = aItems.Value <- aItems.Value |> List.except [item]

//         let! bItems = Mutable.ofValue [ nextNum() ]
//         let addBItem item = aItems.Value <- aItems.Value @ [item]
//         let removeBItem item = aItems.Value <- aItems.Value |> List.except [item]

//         p {
//             button .onclick(fun _ -> addAItem(nextNum())) { "Add A" }
//             button .onclick(fun _ -> addBItem(nextNum())) { "Add B" }
//         }

//         for x in aItems.Value do
//             div .class'("card") .id($"card_{x}") {
//                 button.onclick(fun _ -> removeAItem(x)) { $"Remove {x}" }
//         }
//     }
