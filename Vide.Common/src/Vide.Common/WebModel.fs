module Vide.WebModel

open System
open Vide

()

// TODO: Include "WpfisModel" again and move some MAUI things (i.e. AddElement) to WpfishContext

// TODO: We _could_ also abstract this, but since the HtmlAPI is specialized, I
// don't see no real value in doing so.
//type IWebDocument<'n> =
//    inherit INodeDocument<'n>
//    abstract member CreateNodeOfName : tagName: string -> 'n

module BuilderHelper =
    //let createNode<'e,'n when 'n: equality> tagName (ctx: WebContext<'n>) =
    //    let n = ctx.WebDocument.CreateNodeOfName(tagName)
    //    do ctx.AppendChild(n)
    //    // TODO: Can we get rid of the unsafe cast?
    //    (box n) :?> 'e
    
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
