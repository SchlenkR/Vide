
module Vide.Core

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

type NodeBuilder() = class end

open System.Runtime.CompilerServices

[<Extension>]
type NodeBuilderExtensions() =
    [<Extension>]
    static member inline attrCond(this: #NodeBuilder, value: unit) =
        ()

[<Extension>]
type HTMLAnchorElementBuilderExtensions() =
    [<Extension>]
    static member inline href(this: #NodeBuilder, value) =
        // this is caising the exception on "npm start"
        this.attrCond(value)

        // this is working
        //NodeBuilderExtensions.attrCond(this, "href", ?value = value)
