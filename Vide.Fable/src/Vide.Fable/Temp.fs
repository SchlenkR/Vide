module Temp

open Browser.Types
open Vide
open Vide.Fable
open type Vide.Fable.Html

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

let _ =
    vide {
        let! count1 = Vide.ofMutable<NodeContext<Node,INodeDocument<Node>>, _> 0
        return count1.Value
    }

let _ =
    vide {
        let! count = Vide.ofMutable 0
        div { $"Count = {count.Value}" }
    }
