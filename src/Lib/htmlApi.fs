namespace Vide2

open Browser.Types
open Vide

module HtmlElementBuilders =
    type button() =
        inherit HTMLElementBuilder<HTMLButtonElement>("button")
        member this.id(value) = this.AddModifier(fun x -> x.id <- value)
        member this.className(value) = this.AddModifier(fun x -> x.className <- value)
        member this.hidden(value) = this.AddModifier(fun x -> x.hidden <- value)
        member this.onclick(handler) = this.AddInitOnlyModifier(fun x -> x.onclick <- handler)
    type a() =
        inherit HTMLElementBuilder<HTMLAnchorElement>("a")
        member this.id(value) = this.AddModifier(fun x -> x.id <- value)
        member this.className(value) = this.AddModifier(fun x -> x.className <- value)
        member this.hidden(value) = this.AddModifier(fun x -> x.hidden <- value)
        member this.onclick(handler) = this.AddInitOnlyModifier(fun x -> x.onclick <- handler)
    type span() =
        inherit HTMLElementBuilder<HTMLSpanElement>("a")
        member this.id(value) = this.AddModifier(fun x -> x.id <- value)
        member this.className(value) = this.AddModifier(fun x -> x.className <- value)
        member this.hidden(value) = this.AddModifier(fun x -> x.hidden <- value)
        member this.onclick(handler) = this.AddInitOnlyModifier(fun x -> x.onclick <- handler)
    type div() =
        inherit HTMLElementBuilder<HTMLDivElement>("div")
        member this.id(value) = this.AddModifier(fun x -> x.id <- value)
        member this.className(value) = this.AddModifier(fun x -> x.className <- value)
        member this.hidden(value) = this.AddModifier(fun x -> x.hidden <- value)
        member this.onclick(handler) = this.AddInitOnlyModifier(fun x -> x.onclick <- handler)
    type p() =
        inherit HTMLElementBuilder<HTMLParagraphElement>("p")
        member this.id(value) = this.AddModifier(fun x -> x.id <- value)
        member this.className(value) = this.AddModifier(fun x -> x.className <- value)
        member this.hidden(value) = this.AddModifier(fun x -> x.hidden <- value)
        member this.onclick(handler) = this.AddInitOnlyModifier(fun x -> x.onclick <- handler)

type Html =
    static member inline nothing = HtmlBase.nothing ()
    static member inline text text = HtmlBase.text text
    // ---
    static member inline span = HtmlElementBuilders.span()
    static member inline div = HtmlElementBuilders.div()
    static member inline p = HtmlElementBuilders.p()
    static member inline button = HtmlElementBuilders.button()
    static member inline a = HtmlElementBuilders.a()
