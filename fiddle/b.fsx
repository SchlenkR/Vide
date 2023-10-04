
type MySeqBuilder() =
    member _.Yield(v) = printfn "YIELD"; v
    member _.Combine(a, b) = failwith "WIll never be invoked"
    member _.Delay f = printfn "DELAY"; f()
    member _.Run(f) = printfn "RUN"; f

let mySeq = MySeqBuilder()

let x = mySeq { System.DateTime.Now }





type Vide = Vide of (unit -> int)

type Maybe<'a> = Maybe of (unit -> 'a)

type StateBuilder() =
    member _.Yield(x) =
        printfn "YIELD"
        Vide (fun () -> x)
    member _.Zero() = 
        printfn "ZERO"; 
        Vide (fun () -> 0)
    member _.Combine(Vide a, b) = 
        printfn "COMBINE";
        Vide (fun () ->
            let a = a ()
            let (Vide b) = b ()
            a + b ()
        )
    member _.Delay(f) = 
        printfn "DELAY"; 
        f
    member _.Run(f) = 
        printfn "RUN"; 
        Maybe f

let vide = StateBuilder()

let (Maybe ce) = vide {
    printfn "A"
    System.DateTime.Now.Ticks |> int
    printfn "B"
    System.DateTime.Now.Ticks |> int
    printfn "C"
}

let (Vide v) = ce ()
v ()
