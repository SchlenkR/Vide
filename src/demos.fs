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

let demos : list<string * (HTMLElement -> unit)> = [
    "Hello World", fun host -> helloWorld |> start host
    "Counter", fun host -> counter |> start host
]
