module Vide.WpfishModel

open System
open Vide

type IWpfishDocument<'n when 'n: equality> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfType<'e when 'e : (new: unit -> 'e)> : unit -> 'e * 'n

[<AbstractClass>] 
type WpfishContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: IWpfishDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, document)
    member _.WpfishDocument = document

module BuilderHelper =
    // TODO: This should not append - should be done in "apply"
    let createNode<'e,'n when 'n: equality and 'e : (new: unit -> 'e)> (ctx: WpfishContext<'n>) =
        let e,n = ctx.WpfishDocument.CreateNodeOfType<'e>()
        do ctx.ShowChild(n)
        e

    let inline checkNode(expectedNodeTypeName: string, actualNodeTypeName: string) =
        match actualNodeTypeName.Equals(expectedNodeTypeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false -> DiscardAndCreateNew
