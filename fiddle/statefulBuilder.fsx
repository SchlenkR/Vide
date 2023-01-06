
/// Pseudo custom operation that can be yielded
type Ops =
    | Clear

// Since the typical CE builder is a singleton with keyword-like
// semantics, the builder instance cannot carry it's own state.
// As a workaround, we can model the state (mutable in this case)
// and pass it along the computation (omehow a reader monad-like 
// thing with with unit as result type + mutation).
// The "Run" builder method is then the instanciation of the
// computation with it's mutable context.
type MySeqContext<'a> = { items: ResizeArray<'a> }
type MySeq<'a> = MySeqContext<'a> -> unit

type MySeqBuilder() =
    member _.Bind(m: MySeq<_>, f) : MySeq<_> =
        fun ctx ->
            m ctx
            (f ()) ctx

    member _.Yield(v) : MySeq<_> =
        fun ctx -> ctx.items.Add(v)

    member _.Yield(v: Ops) : MySeq<_> =
        // We use a Yield overload to allow for explicit
        // control of the sequence, since CustomOperations
        // cannot be used in this scenario (yield / combine)
        fun ctx ->
            match v with
            | Clear -> ctx.items.Clear()
    
    member _.Zero() : MySeq<_> = 
        fun ctx -> ()

    member _.Combine(a: MySeq<_>, b : MySeq<_>) : MySeq<_> =
        fun ctx ->
            a ctx
            b ctx

    member _.Delay f : MySeq<_> =
        // The compiler "delays" the yield- and combine
        // results from the bottom of the computation expression
        // to the top, we directly un-delay it.
        // See the picture at the middle-end of Scott Wlaschin's amazing site:
        // https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part3/
        f ()

    member _.Run (f: MySeq<_>) =
        // - After all, Run is called!
        // - It transforms our MySeq to a seq (or whatever)
        //   that is the returned value of the mySeq { .. } expression.
        // - It can also be seen as the "kick-off"- or starting point
        //   for the whole computation.
        let ctx = { items = ResizeArray() }
        f ctx
        ctx.items :> seq<_>

let mySeq = MySeqBuilder()

mySeq {
    1
    2
    Clear
    3
    4
}
