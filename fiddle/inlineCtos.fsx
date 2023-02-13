
type A<'a,^c when ^c : (member Create: (unit -> 'a))>(ctx: ^c) =
    member inline this.CreateNode() = ctx.Create()


type A1<'a>(createNode) =
    inherit A<'a>(createNode)
    member inline this.Create1() = this.Create1()

