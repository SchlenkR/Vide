
type ZeroChild = ZeroChild

type SingleChildBuilder() =
    member _.Yield(v) = v
    member _.Zero() = ZeroChild
    member _.Combine(a: ZeroChild, b) = b
    member _.Delay f = f ()
    member _.Run (f) = f

let single = SingleChildBuilder()

let one = 
    single {
        1
    }

let oneAndTwo = 
    single {
        1
        2
    }
