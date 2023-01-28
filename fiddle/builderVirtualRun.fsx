
module NoRun =

    type Vide<'a> = Vide of seq<'a>

    type BaseBuilder() =
        member _.Zero<'a>() = printfn "ZERO"; Seq.empty<'a> |> Vide
        member _.Delay f = printfn "DELAY"; f()
    
        member _.DoRun(f) = printfn "RUN"; f

    type RunBuilder() =
        inherit BaseBuilder()
        member this.Run(f) = this.DoRun(f)

    let mySeq = RunBuilder()

    let runBuilder (b: #BaseBuilder) : Vide<int> =
        b { () }

    runBuilder mySeq


module Run =

    type Vide<'a> = Vide of seq<'a>

    type BaseBuilder() =
        member _.Zero<'a>() = printfn "ZERO"; Seq.empty<'a> |> Vide
        member _.Delay f = printfn "DELAY"; f()
    
        abstract member Run : Vide<'a> -> Vide<'a>
        default _.Run(f) = printfn "RUN"; f

    type RunBuilder() =
        inherit BaseBuilder()

    let mySeq = RunBuilder()

    let runBuilder (b: #BaseBuilder) : Vide<int> =
        b { () }

    runBuilder mySeq

