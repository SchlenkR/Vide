module App

open Browser.Dom
open Fiu
open Fiu.Fable
open Fiu.Fable.Html

let app = 
    fiu {
        text "Hello World (1)" [] []
        div [] [] {
            text "Hello World (2)" [] []
            p [] []
            p [] []
        }
    }

app |> start (document.getElementById("app"))
