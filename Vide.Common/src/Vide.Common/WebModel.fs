module Vide.WebModel

open Vide

// TODO: Include "WpfisModel" again and move some MAUI things (i.e. AddElement) to WpfishContext

type IWebDocument<'n> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfName : tagName: string -> 'n

[<AbstractClass>] 
type WebContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: IWebDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, document)
    member _.WebDocument = document

module BuilderHelper =
    // TODO: This should not append - should be done in "apply"
    let createNode<'e,'n when 'n: equality> tagName (ctx: WebContext<'n>) =
        let n = ctx.WebDocument.CreateNodeOfName(tagName)
        do ctx.ShowChild(n)
        // TODO: Can we get rid of the unsafe cast?
        (box n) :?> 'e
    
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
