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

module Vide =
    
    // TODO: Move to keywords? / rename to useState?
    let ofMutable x =
        mkVide <| fun s (ctx: HostContext<_>) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, ctx.host))
            s, Some s
