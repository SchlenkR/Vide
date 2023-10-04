[<AutoOpen>]
module Vide.MutableValue

type MutableValue<'a when 'a: equality>(initial: 'a, evalManager: IHost) =
    let mutable state = initial
    member _.Set(value) =
        // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
        if value <> state then
            do state <- value
            do evalManager.RequestEvaluation()
    member this.Reset() = this.Set(initial)
    member inline this.Update(op, value) =
        this.Value <- op this.Value value
    member this.Value
        with get() = state
        and set(value) = this.Set(value)

type StateCtor<'a> = StateCtor of (unit -> 'a)

type VideBaseBuilder with
    member _.Bind(StateCtor m, f) =
        mkVide <| fun s ctx ->
            let m = m ()
            let bindRes = BuilderBricks.bind(m, f)
            runVide bindRes s ctx

type DelayedMutableBuilder() =
    member _.Yield(x) =
        mkVide <| fun s (ctx: HostContext<_>) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, ctx.host))
            s, Some s
    member _.Zero() = BuilderBricks.zeroAdaptiveState
    member _.Combine(a, b) = BuilderBricks.combine(a, b ())
    member _.Delay(f) = f
    member _.Run(f) = StateCtor f

let ofMutable = DelayedMutableBuilder()
