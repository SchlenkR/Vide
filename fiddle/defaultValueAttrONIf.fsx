
type BaseBuilder() =
    member _.Bind(m, f) = match m with | Some m -> f m | None -> None
    member _.Return(x) = Some x
    
    member _.Zero() = ()
    
    [<DefaultValue>]
    member _.Zero() = None

let maybe = BaseBuilder()

let maybeInt = maybe {
    if true then
        return 1
}
