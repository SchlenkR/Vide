module App

open Browser.Dom
open Fiu
open Fiu.Fable.Html

let app =
    fiu {
        text "Hello World (1)" [] []
        div [] [] {
            let! count = state 10
            text $"Hello World ({count.Value})" [] []
            button [] [("click", fun e -> count.Value <- count.Value + 1)] { "inc" }
            button [] [("click", fun e -> count.Value <- count.Value - 1)] { "dec" }
            "Text only"
        }
    }

app |> start (document.getElementById("app"))
