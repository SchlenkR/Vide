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

#if EXCLUDE_MUTABLE_OPERATORS
    // This is for websharper compilation!
#else
    // Operators
    static member inline ( + ) (this: MutableValue<'v>, v: 'v) = this.Value + v
    static member inline ( - ) (this: MutableValue<'v>, v: 'v) = this.Value - v
    static member inline ( / ) (this: MutableValue<'v>, v: 'v) = this.Value / v
    static member inline ( * ) (this: MutableValue<'v>, v: 'v) = this.Value * v
    static member inline ( % ) (this: MutableValue<'v>, v: 'v) = this.Value % v
    
// TODO: Do I really want this / AutoOpen and the ops at all?
let inline ( += ) (mutVal: MutableValue<_>) x = mutVal.Update((+), x)
let inline ( -= ) (mutVal: MutableValue<_>) x = mutVal.Update((-), x)
let inline ( *= ) (mutVal: MutableValue<_>) x = mutVal.Update((*), x)
let inline ( /= ) (mutVal: MutableValue<_>) x = mutVal.Update((/), x)
let inline ( := ) (mutVal: MutableValue<_>) x = mutVal.Value <- x
    
// TODO: override arithmetic ops

#endif

module Vide =
    
    // TODO: Move to keywords? / rename to useState?
    let ofMutable x =
        Vide <| fun s host ctx ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, host))
            s, Some s
