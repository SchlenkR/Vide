
type MySeqBuilder() =
    member _.Yield(v) = printfn "YIELD"; Seq.singleton v
    member _.Zero<'a>() = printfn "ZERO"; Seq.empty<'a>
    member _.Combine(a, b) = printfn "COMB"; Seq.append a b
    member _.Delay f = printfn "DELAY"; f()
    member _.Run(f) = printfn "RUN"; f

let mySeq = MySeqBuilder()

let a: seq<int> = 
    mySeq { () }

let b: seq<int> = 
    mySeq { 1 }

let b =
    mySeq {
        1
        //2
        //3
    }
