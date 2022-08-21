
namespace Vide

open System.Runtime.CompilerServices
open Browser.Types
open Vide

type HTMLElementBuilder(createNode, updateNode) = inherit NodeBuilder(createNode, updateNode)
type HTMLAnchorElementBuilder(createNode, updateNode) = inherit HTMLElementBuilder(createNode, updateNode)
type HTMLButtonElementBuilder(createNode, updateNode) = inherit HTMLElementBuilder(createNode, updateNode)

[<Extension>]
type NodeBuilderExtensions() =
    [<Extension>]
    static member inline attrCond(this: #NodeBuilder, name, ?value: string) =
        let value =
            match value with
            | Some value -> Set value
            | None -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member inline attrBool(this: #NodeBuilder, name, value: bool) =
        let value =
            match value with
            | true -> Set ""
            | false -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member on(this: #NodeBuilder, name, handler: EventHandler) =
        do this.Events <- (name, handler) :: this.Events
        this

[<Extension>]
type HTMLElementBuilderExtensions() =
    [<Extension>]
    static member inline id(this: #HTMLElementBuilder, ?value) =
        NodeBuilderExtensions.attrCond(this, "id", ?value = value)
    [<Extension>]
    static member inline class'(this: #HTMLElementBuilder, ?value) =
        NodeBuilderExtensions.attrCond(this, "class", ?value = value)
    [<Extension>]
    static member inline hidden(this: #HTMLElementBuilder, value) =
        NodeBuilderExtensions.attrBool(this, "hidden", value)
    
    [<Extension>]
    static member inline onclick(this: #HTMLElementBuilder, handler) =
        NodeBuilderExtensions.on(this, "click", handler)

[<Extension>]
type HTMLAnchorElementBuilderExtensions() =
    [<Extension>]
    static member inline href(this: #HTMLAnchorElementBuilder, ?value: string) =
        // Fable BUG https://github.com/fable-compiler/Fable/issues/3073
        NodeBuilderExtensions.attrCond(this, "href", ?value = value)

module Element =
    let inline create tagName htmlElementBuilderCtor =
        let create ctx = ctx.elementsContext.AddElement(tagName) :> Node
        let update (node: Node) = Keep
        htmlElementBuilderCtor(create, update)
    
// open type (why? -> We need always a new builder)
type Html =
    static member text<'s> text =
        NodeBuilder(
            (fun ctx -> ctx.elementsContext.AddTextNode(text) :> Node),
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
        member inline _.Yield(
            v: NodeBuilder)
            : Vide<unit, NodeBuilderState<unit>, Context>
            =
            v { () }
        member inline _.Yield(
            x: string)
            : Vide<unit, NodeBuilderState<unit> ,Context>
            =
            Html.text x { () }
