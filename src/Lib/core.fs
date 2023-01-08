[<AutoOpen>]
module Vide.Core

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

module Debug =
    let mutable enabledDebugChannels : int list = []
    let print channel s = if enabledDebugChannels |> List.contains channel then printfn "%s" s

let inline internal separateStatePair s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

// Preserves the first value given and discards subsequent values.
let preserve x =
    Vide <| fun s ctx ->
        let s = s |> Option.defaultValue x
        s, Some s

let preserveWith x =
    Vide <| fun s ctx ->
        let s = s |> Option.defaultWith x
        s, Some s

// TODO: Think about which function is "global" and module-bound
let map (proj: 'v1 -> 'v2) (Vide v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
    Vide <| fun s ctx ->
        let v,s = v s ctx
        proj v, s


// why 's and not unit? -> see comment in "VideBuilder.Zero"
let inline zero<'s,'c> : Vide<unit,'s,'c> =
    Vide <| fun s ctx -> (),None

let inline nothing<'c> = zero<unit,'c>

[<AbstractClass>]
type VideContext() =
    abstract member RequestEvaluation: unit -> unit

type ComputationState<'v> =
    {
        prom: Async<'v>
        result: Ref<'v option>
    }

type VideBuilder() =
    member inline _.Bind
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s ctx ->
            Debug.print 0 "BIND"
            let ms,fs = separateStatePair s
            let mv,ms = m ms ctx
            let (Vide f) = f mv
            let fv,fs = f fs ctx
            fv, Some (ms,fs)
    member _.Bind<'v1,'v2,'s2,'c when 'c :> VideContext>
        (
            m: Async<'v1>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) : Vide<'v2, ComputationState<'v1> * 's2 option, 'c>
        =
        // If Bind is marked inline, runtime errors can occur when using
        // "do!" or "let! _ =" (seems to be a bug in fable compiler).
        Vide <| fun s ctx ->
            let fv,ms,fs =
                match s with
                | None ->
                    let result = ref None
                    let onsuccess res =
                        Debug.print 1 $"awaited result: {res}"
                        do result.Value <- Some res
                        do ctx.RequestEvaluation()
                    // TODO: global cancellation handler / ex / cancellation, etc.
                    let onexception ex = ()
                    let oncancel ex = ()
                    Async.StartWithContinuations(m, onsuccess, onexception, oncancel)
                    Unchecked.defaultof<'v2>, { prom = m; result = result }, None
                | Some (comp,fs) ->
                    match comp.result.Value with
                    | Some mres ->
                        let (Vide f) = f mres
                        let fv,fs = f fs ctx
                        fv,comp,fs
                    | None -> Unchecked.defaultof<'v2>,comp,fs
            fv, Some (ms,fs)
    member inline _.Return
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        Vide <| fun s ctx -> x,None
    
    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // Unfortunately, we cannot have both. For that reason, "if"s without "else"
    // must use "else elseZero".
    member inline _.Zero
        ()
        : Vide<unit,unit,'c>
        = zero<unit,'c>
    member inline _.Delay
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        Debug.print 0 "DELAY"
        f()
    member inline _.Combine
        (
            Vide a: Vide<'unit,'s1,'c>,
            Vide b: Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s ctx ->
            Debug.print 0 "COMBINE"
            let sa,sb = separateStatePair s
            let va,sa = a sa ctx
            let vb,sb = b sb ctx
            vb, Some (sa,sb)
    member inline _.For
        (
            input: seq<'a>,
            body: 'a -> Vide<'v,'s,'c>
        ) 
        : Vide<'v list, Map<'a, 's option>,'c>
        = 
        Vide <| fun s ctx ->
            Debug.print 0 "FOR"
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for x in input do
                    let matchingState = currMap |> Map.tryFind x |> Option.flatten
                    let v,s = let (Vide v) = body x in v matchingState ctx
                    do currMap <- currMap |> Map.remove x
                    v, (x,s)
                ]
                |> List.unzip
            resValues, Some (resStates |> Map.ofList)

let vide = VideBuilder()

module Mutable =
    type MutableValue<'a when 'a: equality>(init: 'a, requestEvaluation: unit -> unit) =
        let mutable state = init
        member _.Set(value) =
            // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
            // No -> not true anymore since triggered evaluations are modeled as subsequent
            // (non-accumulating) requests; so it's "only" a perf-op.
            // TODO: Without that opt, we could get rid of equality constraint. Think about it...
            if value <> state then
                do state <- value
                do requestEvaluation()
        member this.Value
            with get() = state
            and set(value) = this.Set(value)
        // TODO: override arithmetic ops

    let inline change op (mutVal: MutableValue<_>) x =
        mutVal.Value <- op mutVal.Value x
    
    let ofValue x =
        Vide <| fun s (c: #VideContext) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, c.RequestEvaluation))
            s, Some s

// TODO: Do I really want this?
let inline ( += ) mutVal x = Mutable.change (+) mutVal x
let inline ( -= ) mutVal x = Mutable.change (-) mutVal x
let inline ( *= ) mutVal x = Mutable.change (*) mutVal x
let inline ( /= ) mutVal x = Mutable.change (/) mutVal x
let inline ( := ) (mutVal: Mutable.MutableValue<_>) x = mutVal.Value <- x
