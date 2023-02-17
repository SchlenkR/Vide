module Vide.WebModel

open System
open Vide

// TODO: Include "WpfisModel" again and move some MAUI things (i.e. AddElement) to WpfishContext

type IWebDocument<'n> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfName : tagName: string -> 'n

[<AbstractClass>] 
type WebContext<'n when 'n: equality>
    (
        parent: 'n, 
        evaluationManager: IEvaluationManager,
        document: IWebDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, evaluationManager, document)
    member this.AddNodeOfName<'e>(tagName: string) : 'e =
        let n = document.CreateNodeOfName(tagName)
        do this.KeepChild(n)
        do document.AppendChild(parent, n)
        // TODO: Can we get rid of the unsafe cast?
        (box n) :?> 'e

module BuilderHelper =
    let createNode elemName (ctx: #WebContext<_>) =
        ctx.AddNodeOfName<'n>(elemName)
    
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
