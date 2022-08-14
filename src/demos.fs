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
        text "Hello XXX World (1)"
        div {
            let! count = state 10
            let incDec i _ = 
                printfn $"INC/DEC  {i}"
                count.value <- count.value + i

            text $"Hello World ({count.value})"
            button.on("click", incDec -1) {
                "dec"
                div { "yyy" }
            }
            button.on("click", incDec 1) { "inc" }
            "Text only<strong>HALLO</strong>"
            
            //span.hidden(?value = if count.value % 2 = 0 then Some true else None) { "ODD" }
        }
    }

let demos : list<string * (HTMLElement -> unit)> = [
    "Hello World", fun host -> helloWorld |> start host
    "Counter", fun host -> counter |> start host
]
