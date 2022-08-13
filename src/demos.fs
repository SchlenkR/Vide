module Demos

open Browser.Dom
open Browser.Types
open Vide

// TODO: so many opens -- that sucks
open type Vide.Fable.Dom.Attributes
open type Vide.Fable.Dom.Events
open type Vide.Fable.Dom.Elements

let helloWorld =
    vide {
        text "Hello World"
    }

let counter =
    vide {
        text "Hello World (1)"
        div {
            let! count = Mutable.value 10
            text $"Hello World ({count.Value})"
            
            let b1 = button ^+ onclick (fun e -> count.Value <- count.Value + 1)
            b1 { "dec" }

            button ^+ onclick (fun e -> count.Value <- count.Value + 1) { "inc" }

            "Text only"
        }
    }

let demos : list<string * (HTMLElement -> unit)> = [
    "Hello World", fun host -> helloWorld |> start host
    "Counter", fun host -> counter |> start host
]
