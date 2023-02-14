[<AutoOpen>]
module Vide.WebModel

open System
open Vide

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

type IWebDocument<'n> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfName : tagName: string -> 'n
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

[<AbstractClass>] 
type WebContext<'n when 'n: equality>
    (
        parent: 'n, 
        evaluationManager: IEvaluationManager,
        document: IWebDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, evaluationManager, document)

    let appendToParent child = document.AppendChild(parent, child)

    member this.AddNodeOfName<'e>(tagName: string) : 'e =
        let n = document.CreateNodeOfName(tagName)
        do this.KeepChild(n)
        do n |> appendToParent 
        // TODO: Can we get rid of the unsafe cast?
        (box n) :?> 'e
    member this.AddTextNode(text: string) =
        let t = document.CreateTextNode(text)
        do this.KeepChild(t.node)
        do t.node |> appendToParent
        t

module BuilderBricks =
    let createNode elemName (ctx: #WebContext<_>) =
        ctx.AddNodeOfName<'n>(elemName)
    
    let checkOrUpdateNode expectedNodeName (actualNodeName: string) =
        match actualNodeName.Equals(expectedNodeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
    
    let inline yieldText<'nc,'c when 'c :> WebContext<'nc>>(value: string) =
        Vide <| fun s (ctx: 'c) ->
            let textNode = s |> Option.defaultWith (fun () -> ctx.AddTextNode(value))
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.KeepChild(textNode.node)
            (), Some textNode
