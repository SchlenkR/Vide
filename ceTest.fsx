
type Builder() =
    member inline this.Yield(x) = [x]
    member inline _.Delay(f: unit -> _) = f
    member _.Combine(a, b) = List.append a (b())
    member _.Zero() = []
    member _.Run(children) = children()
    member inline _.For(inputList: seq<'a>, body: 'a -> list<'b>) : seq<list<'b>> =
        [ for x in inputList do body x ]
let test = Builder()

test { 
    for x in 0..10 do
        "xxx"
}
