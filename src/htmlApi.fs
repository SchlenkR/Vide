
namespace Vide

open System
open System.Runtime.CompilerServices
open Fable.Core.JS
open Browser.Types
open Vide

type HTMLElementBuilder<'n when 'n :> Node>(createNode, updateNode) =
    inherit NodeBuilder<'n>(createNode, updateNode)
type HTMLAnchorElementBuilder(createNode, updateNode) =
    inherit HTMLElementBuilder<HTMLAnchorElement>(createNode, updateNode)
type HTMLButtonElementBuilder(createNode, updateNode) =
    inherit HTMLElementBuilder<HTMLButtonElement>(createNode, updateNode)

[<Extension>]
type NodeBuilderExtensions() =
    [<Extension>]
    static member inline attrCond(this: #NodeBuilder<_>, name, ?value: string) =
        let value =
            match value with
            | Some value -> Set value
            | None -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member inline attrBool(this: #NodeBuilder<_>, name, value: bool) =
        let value =
            match value with
            | true -> Set ""
            | false -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member on(this: #NodeBuilder<_>, name, handler: EventHandler) =
        do this.Events <- (name, handler) :: this.Events
        this

[<Extension>]
type HTMLElementBuilderExtensions() =
    [<Extension>]
    static member inline id(this: #HTMLElementBuilder<_>, ?value) =
        NodeBuilderExtensions.attrCond(this, "id", ?value = value)
    [<Extension>]
    static member inline class'(this: #HTMLElementBuilder<_>, ?value) =
        NodeBuilderExtensions.attrCond(this, "class", ?value = value)
    [<Extension>]
    static member inline hidden(this: #HTMLElementBuilder<_>, value) =
        NodeBuilderExtensions.attrBool(this, "hidden", value)
    
    [<Extension>]
    static member inline onclick(this: #HTMLElementBuilder<_>, handler) =
        NodeBuilderExtensions.on(this, "click", handler)

[<Extension>]
type HTMLAnchorElementBuilderExtensions() =
    [<Extension>]
    static member inline href(this: #HTMLAnchorElementBuilder, ?value: string) =
        // Fable BUG https://github.com/fable-compiler/Fable/issues/3073
        NodeBuilderExtensions.attrCond(this, "href", ?value = value)

module Element =
    let inline create<'n,'b when 'n :> HTMLElement and 'b :> HTMLElementBuilder<'n>>
        tagName
        htmlElementBuilderCtor
        : 'b
        =
        let create ctx = ctx.elementsContext.AddElement<'n>(tagName)
        let update (node: 'n) =
            match node.nodeName.Equals(tagName, StringComparison.OrdinalIgnoreCase) with
            | true -> Keep
            | false ->
                console.log($"TODO: if/else detection? Expected node name: {tagName}, but was: {node.nodeName}")
                DiscardAndCreateNew
        htmlElementBuilderCtor(create, update)
    
// open type (why? -> We need always a new builder)
type Html =
    static member text text =
        NodeBuilder(
            (fun ctx -> ctx.elementsContext.AddTextNode(text)),
            (fun node ->
                if typeof<Text>.IsInstanceOfType(node) then
                    if node.textContent <> text then node.textContent <- text
                    Keep
                else
                    DiscardAndCreateNew
            ))
    static member span = Element.create "span" HTMLElementBuilder
    static member div = Element.create "div" HTMLElementBuilder
    static member p = Element.create "p" HTMLElementBuilder
    static member button = Element.create "button" HTMLButtonElementBuilder
    static member a = Element.create "a" HTMLAnchorElementBuilder

    // TODO: Yield should work for strings

[<AutoOpen>]
module VideBuilderExtensions =
    type VideBuilder with
        member inline _.Yield
            (x: Vide<'v,'s,'c>)
            : Vide<'v,'s,'c>
            =
            x
        member inline _.Yield
            (v: NodeBuilder<_>)
            : Vide<unit, NodeBuilderState<unit,_>, Context>
            =
            v { () } |> map ignore
        member inline _.Yield
            (x: string)
            : Vide<unit, NodeBuilderState<unit,_> ,Context>
            =
            Html.text x { () } |> map ignore
