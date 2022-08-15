module Demos

open Vide

open type Vide.Fable.Html

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
        let add1 _ = currentItems.Value @ [nextNum()] |> setItems
        let add100 _ = currentItems.Value @ [ for _ in 0..100 do nextNum() ] |> setItems
        let removeAll _ = setItems []

        button .onclick(add1) { "Add One" }
        button .onclick(add100) { "Add 100" }
        button .onclick(removeAll) { "Remove All" }
        
        for x in currentItems.Value do
            div .class'("card") {
                let removeMe _ = currentItems.Value |> List.except [x] |> setItems
                button .onclick(removeMe) { $"Remove {x}" }
        }
    }

let stateInForLoop =
    vide {
        let! currentItems = Mutable.ofValue []
        let setItems items = currentItems.Value <- items
        let add1 _ = currentItems.Value @ [nextNum()] |> setItems
        let add100 _ = currentItems.Value @ [ for _ in 0..100 do nextNum() ] |> setItems
        let removeAll _ = setItems []

        button .onclick(add1) { "Add One" }
        button .onclick(add100) { "Add 100" }
        button .onclick(removeAll) { "Remove All" }
        
        for x in currentItems.Value do
            let! count = Mutable.ofValue 0
            div .class'("card") {
                let removeMe _ = currentItems.Value |> List.except [x] |> setItems
                button .onclick(removeMe) { $"Remove {x}" }

                button .onclick(fun _ -> count -= 1) { "dec" }
                text $"{count.Value}  "
                button .onclick(fun _ -> count += 1) { "inc" }
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
