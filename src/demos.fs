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
        let incDec i _ = count.value <- count.value + i

        div {
            text $"Count = {count.value}"
        }

        button.onclick(incDec -1) { "dec" }
        button.onclick(incDec 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button.on("click", fun _ -> count.value <- count.value + 1) {
            $"Hit me! Count = {count.value}"
        }
        div.class'("the-message") {
            span.hidden(count.value <> 5) {
                "You have the right to defend yourself!"
            }
        }
    }

let lists =
    vide {
        for x in 0..5 do
            div.class'("card") { $"I'm element no. {x}" }
    }

let nextNum() = System.Random().Next(10000)

let mutableLists =
    vide {
        div {
            let! currentItems = Mutable.ofValue [ nextNum() ]
            let addItem item =
                printfn "ADD"
                currentItems.value <- currentItems.value @ [item]
            let removeItem item = currentItems.value <- currentItems.value |> List.except [item]

            button.onclick(fun _ -> addItem(nextNum())) { "Add" }
            for x in currentItems.value do
                div.class'("card").id($"card_{x}") {
                    button.onclick(fun _ -> removeItem(x)) { $"Remove {x}" }
            }
        }
    }
