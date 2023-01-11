
type MySeqBuilder() =
    member _.Zero() = Seq.empty
let mySeq = MySeqBuilder()

let a: seq<'a> = seq { () }
let a1 = a |> Seq.map ((+) 1)
let a2 = a |> Seq.map ((+) 1.0)

let b: seq<'a> = Seq.empty
let b1 = b |> Seq.map ((+) 1)
let b2 = b |> Seq.map ((+) 1.0)
