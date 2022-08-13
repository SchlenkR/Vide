module HtmlDslTest01

open Browser.Dom
open Browser.Types
open Vide

open type Vide.Fable.Dom


let counter =
    vide {
        text "Hello World (1)"
        div {
            let! count = mval 10
            text $"Hello World ({count.Value})"
            button [] [ ("click", fun e -> count.Value <- count.Value - 1) ] { "dec" }
            button [] [ ("click", fun e -> count.Value <- count.Value + 1) ] { "inc" }
            "This is a <strong>wonderful</strong> world."
        }
    }
