module HtmlDslTest01

open Browser.Dom
open Browser.Types
open Fiu
open Fiu.Fable.Html

type HtmlBuilder<'fs1,'fs2,'c>(
    run: Fiu<unit,'fs1,'c> -> Fiu<unit,'fs2,'c>)
    =
    inherit FiuBuilder<'fs1,'fs2,'c>(run)

    let mutable attributes = Map.empty
    member this.Attributes = attributes
    member this.AddAttribute(name: string, value: string) =
        attributes <- attributes |> Map.add name value

    static member (@) (b : HtmlBuilder<'fs1,'fs2,'c>, attr : string * string) =
      b.AddAttribute(attr)

open System

type Html =
    static member text
        with get () = DateTime.Now.Ticks
    

open type Html


let counter =
    fiu {
        text "Hello World (1)" [] []
        div [] [] {
            let! count = state 10
            text $"Hello World ({count.Value})" [] []
            button [] [ ("click", fun e -> count.Value <- count.Value - 1) ] { "dec" }
            button [] [ ("click", fun e -> count.Value <- count.Value + 1) ] { "inc" }
            "Text only"
        }
    }
