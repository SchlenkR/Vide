namespace Vide

open System.Runtime.CompilerServices
open Browser.Types
open Vide
open HtmlElementBuilders

[<Extension>]
type BuilderExtensions =
    
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
        this.OnEval(fun x -> x.node.setAttribute(key, value))
    
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
