
namespace Vide

open System
open System.Runtime.CompilerServices
open Fable.Core.JS
open Browser.Types
open Vide
open Vide.Fable

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
    let inline create<'n,'b
                when 'n :> HTMLElement 
                and 'b :> HTMLElementBuilder<'n>>
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
    
// open type (why? -> We need always a new builder on property access)
type Html =
    // TODO: This is something special
    static member inline nothing =
        NodeBuilder(
            (fun ctx -> ctx.elementsContext.DummyElement()),
            (fun node -> Keep))
    // TODO: This is something special
    static member inline text text =
        NodeBuilder(
            (fun ctx -> ctx.elementsContext.AddTextNode(text)),
            (fun node ->
                if typeof<Text>.IsInstanceOfType(node) then
                    if node.textContent <> text then node.textContent <- text
                    Keep
                else
                    DiscardAndCreateNew
            ))
    static member inline span = Element.create "span" HTMLElementBuilder
    static member inline div = Element.create "div" HTMLElementBuilder
    static member inline p = Element.create "p" HTMLElementBuilder
    static member inline button = Element.create "button" HTMLButtonElementBuilder
    static member inline a = Element.create "a" HTMLAnchorElementBuilder

    // TODO: Yield should work for strings

[<AutoOpen>]
module VideBuilderExtensions =
    type VideBuilder with
        member inline this.Bind
            (
                x: NodeBuilder<'n>,
                f: 'n -> Vide<'v,'s1,Context>
            ) : Vide<'v,NodeBuilderState<'n,unit> option * 's1 option,Context>
            =
            let v = x { () }
            this.Bind(v, f)
        //member inline _.Yield
        //    (v: NodeBuilder<'n>)
        //    : Vide<unit, NodeBuilderState<'n,_>, Context>
        //    =
        //    v { () } |> map ignore
        member inline _.Yield<'n,'s,'c when 'n :> Node>
            (v: Vide<'n,'s,'c>)
            : Vide<unit,'s,'c>
            =
            v |> map ignore
        member inline _.Yield
            (x: NodeBuilder<'n>)
            : Vide<unit, NodeBuilderState<'n,unit> ,Context>
            =
            x { () } |> map ignore
        member inline _.Yield
            (x: string)
            : Vide<unit, NodeBuilderState<Text,unit> ,Context>
            =
            Html.text x { () } |> map ignore
