module Demos

open Browser.Dom
open Browser.Types
open Vide
open Vide.Fable.Html

let helloWorld =
    vide {
        text "Hello World" [] []
    }

let counter =
    vide {
        text "Hello World (1)" [] []
        div [] [] {
            let! count = state 10
            text $"Hello World ({count.Value})" [] []
            button [] [ ("click", fun e -> count.Value <- count.Value - 1) ] { "dec" }
            button [] [ ("click", fun e -> count.Value <- count.Value + 1) ] { "inc" }
            "Text only"
        }
    }

let demos : list<string * (HTMLElement -> unit)> = [
    "Hello World", fun host -> helloWorld |> start host
    "Counter", fun host -> counter |> start host
]
