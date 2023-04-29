namespace Vide

open System.Runtime.CompilerServices
open Browser.Types
open Vide
open HtmlElementBuilders

[<Extension>]
type BuilderExtensions =
    
    // TODO: MemLeaks
    /// Registers the an event handler.
    [<Extension>]
    static member on<'nb,'e,'n
            when 'nb :> NodeBuilder<'e,'n,FableContext>
            and 'e :> HTMLElement 
            and 'n :> Node>
        (
            this: 'nb,
            eventName: string,
            handler: (Event -> unit)
        ) =
        this.onInit(fun x -> x.node.addEventListener(eventName, handler))
    
    /// Sets an arbitrary attribute's value on every eval cycle.
    [<Extension>]
    static member attr<'nb,'e,'n
            when 'nb :> NodeBuilder<'e,'n,FableContext>
            and 'e :> HTMLElement 
            and 'n :> Node>
        (
            this: 'nb,
            key: string,
            value: string
        ) =
        this.onEval(fun x -> x.node.setAttribute(key, value))
    
    /// Sets an arbitrary boolean attribute's value on every eval cycle.
    [<Extension>]
    static member attrBoolean<'nb,'e,'n
            when 'nb :> NodeBuilder<'e,'n,FableContext>
            and 'e :> HTMLElement 
            and 'n :> Node>
        (
            this: 'nb,
            key: string,
            value: bool
        ) =
        this.onEval(fun x -> 
            if value
            then x.node.setAttribute(key, null) 
            else x.node.removeAttribute(key))
    
    /// Sets the value of a 'data-' attribute on every eval cycle.
    [<Extension>]
    static member data<'nb,'e,'n
            when 'nb :> NodeBuilder<'e,'n,FableContext>
            and 'e :> HTMLElement 
            and 'n :> Node>
        (
            this: 'nb,
            key: string,
            value: string
        ) =
        this.attr($"data-{key}", value)

[<AutoOpen>]
module CustomElements =
    /// The `e` function creates an element with the given tag name.
    /// It provides all global attributes and events, and it's possible to set attributes 
    /// using `attr` / `attrBoolean` or register event handlers using `on`.
    let e (tagName: string) = HtmlGARenderRetCnBuilder<HTMLElement>(tagName)
