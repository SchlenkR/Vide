
module Vide.Core

open System.Runtime.CompilerServices

type NodeBuilder() = class end

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
        // 
        this.attrCond(value)

        // this is working
        //NodeBuilderExtensions.attrCond(this, "href", ?value = value)
