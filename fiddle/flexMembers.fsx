
open System.Runtime.CompilerServices

type Node = class end

type BaseBuilder<'a when 'a :> Node>() =
    class end

type SpecialBuilder<'a when 'a :> Node>() =
    inherit BaseBuilder<'a>()

[<Extension>]
type BaseBuilderExtensions =
    //[<Extension>] static member id<'a, 'b when 'b :> BaseBuilder<'a>>(this: 'b, value: string) : 'b = this
    //[<Extension>] static member className<'a, 'b when 'b :> BaseBuilder<'a>>(this: 'b, value: string) : 'b = this
    [<Extension>] static member id(this: #BaseBuilder<_>, value: string) = this
    [<Extension>] static member className(this: #BaseBuilder<_>, value: string) = this

let specialBuilderInst = SpecialBuilder<Node>()

let fluent : SpecialBuilder<Node> = specialBuilderInst.id("xxx")

