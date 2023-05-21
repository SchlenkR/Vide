
type Component =
    static member Page(?content: seq<'a>) : 'a = failwith ""

let res = Component.Page()
