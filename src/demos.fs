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
        div {
            let! count = state 0
            let incDec i _ = count.value <- count.value + i

            div {
                text $"Count = {count.value}"
            }

            button.on("click", incDec -1) { "dec" }
            button.on("click", incDec 1) { "inc" }
        }
    }

let conditionalAttributes =
    vide {
        div {
            let! count = state 0

            button.on("click", fun _ -> count.value <- count.value + 1) {
                $"Hit me! Count = {count.value}"
            }
            div.class'("the-message") {
                span.hidden(count.value <> 5) {
                    "You have the right to defend yourself!"
                }
            }
        }
    }
