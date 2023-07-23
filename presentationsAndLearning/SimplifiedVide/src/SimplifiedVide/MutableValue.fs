[<AutoOpen>]
module Vide.MutableValue

type MutableValue<'a when 'a: equality>(initial: 'a, app: IApp) =
    let mutable state = initial
    member _.Set(value) =
        // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
        if value <> state then
            do state <- value
            do app.RequestEvaluation()
    member this.Reset() = this.Set(initial)
    member inline this.Update(op, value) =
        this.Value <- op this.Value value
    member this.Value
        with get() = state
        and set(value) = this.Set(value)

module Vide =
    let ofMutable x =
        Vide <| fun s app ctx ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, app))
            s,s
