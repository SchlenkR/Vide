[<AutoOpen>]
module Vide.Core

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

module Debug =
    let mutable enabledDebugChannels : int list = []
    let print channel s = if enabledDebugChannels |> List.contains channel then printfn "%s" s

let inline separateStatePair s =
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
[<GeneralizableValue>]
let zero<'s,'c> : Vide<unit,'s,'c> =
    Vide <| fun s ctx -> (),None

[<GeneralizableValue>]
let context<'c> : Vide<'c,unit,'c> =
    Vide <| fun s ctx -> ctx,None

[<GeneralizableValue>]
let nothing<'c> = zero<unit,'c>
     
[<AbstractClass>]
type VideContext() =
    abstract member RequestEvaluation: unit -> unit

type ComputationState<'v> =
    {
        startedWorker: Async<'v>
        result: Ref<'v option>
    }

type AsyncBindResult<'v1,'v2> = 
    AsyncBindResult of comp: Async<'v1> * cont: ('v1 -> 'v2)
   
type VideBuilder() =
    member _.Bind
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

    member _.Return
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        Vide <| fun s ctx -> x,None

    member _.Yield<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        Debug.print 0 "YIELD Vide"
        v

    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // Unfortunately, we cannot have both. For that reason, "if"s without "else"
    // must use "else elseZero".
    member _.Zero
        ()
        : Vide<unit,unit,'c>
        = zero<unit,'c>

    member _.Delay
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        Debug.print 0 "DELAY"
        f()

    // those 2 are important for being able to combine
    // HTML elements, returns and asyncs in arbitrary order
    //member _.Combine
    //    (
    //        a: Vide<unit,'s1,'c>,
    //        b: Vide<'v,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b snd
    //member _.Combine
    //    (
    //        a: Vide<'v,'s1,'c>,
    //        b: Vide<unit,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b fst
    member _.Combine
        (
            Vide a: Vide<'v1,'s1,'c>,
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

    member _.For
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

    // ---------------------
    // ASYNC
    // ---------------------
    
    member _.Bind<'v1,'v2,'s,'c>
        (
            m: Async<'v1>,
            f: 'v1 -> Vide<'v2,'s,'c>
        ) : AsyncBindResult<'v1, Vide<'v2,'s,'c>>
        =
        Debug.print 0 "BIND async"
        AsyncBindResult(m, f)
    
    member _.Delay
        (f: unit -> AsyncBindResult<'v1,'v2>)
        : AsyncBindResult<'v1,'v2>
        =
        Debug.print 0 "DELAY async"
        f()
    
    member _.Combine<'v,'x,'s1,'s2,'c when 'c :> VideContext>
        (
            Vide a: Vide<'v,'s1,'c>,
            b: AsyncBindResult<'x, Vide<'v,'s2,'c>>
        )
        : Vide<'v, 's1 option * ComputationState<_> option * 's2 option, 'c>
        =
        Vide <| fun s ctx ->
            let sa,comp,sb =
                match s with
                | None -> None,None,None
                | Some (sa,comp,sb) -> sa,comp,sb
            // TODO: Really reevaluate here at this place?
            let va,sa = a sa ctx
            let v,comp,sb =
                match comp with
                | None ->
                    let (AsyncBindResult (comp,_)) = b
                    let result = ref None
                    do
                        let onsuccess res =
                            Debug.print 1 $"awaited result: {res}"
                            do result.Value <- Some res
                            do ctx.RequestEvaluation()
                        // TODO: global cancellation handler / ex / cancellation, etc.
                        let onexception ex = ()
                        let oncancel ex = ()
                        do Async.StartWithContinuations(comp, onsuccess, onexception, oncancel)
                    let comp = { startedWorker = comp; result = result }
                    va,comp,None
                | Some comp ->
                    match comp.result.Value with
                    | Some v ->
                        let (AsyncBindResult (_,f)) = b
                        let (Vide b) = f v
                        let vb,sb = b sb ctx
                        vb,comp,sb
                    | None ->
                        va,comp,sb
            v, Some (sa, Some comp, sb)
    

let vide = VideBuilder()

module Mutable =
    type MutableValue<'a when 'a: equality>(init: 'a, requestEvaluation: unit -> unit) =
        let mutable state = init
        member _.Set(value) =
            // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
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

type VideApp<'v,'s,'c when 'c :> VideContext>
    (
        content: Vide<'v,'s,'c>,
        ctxCtor: ('c -> unit) -> 'c,
        onEvaluated: 'v -> 's option -> unit
    ) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0
        
    let (Vide content) = content
    let evaluate ctx =
        // During an evaluation, requests for another evaluation can
        // occur, which have to be handled as _subsequent_ evaluations!
        let rec eval () =
            do isEvaluating <- true
            let value,newState = content currentState ctx
            do
                currValue <- Some value
                currentState <- newState
            do onEvaluated value currentState
            do isEvaluating <- false
            Debug.print 10 $"Evaluation done ({evaluationCount})"
            if hasPendingEvaluationRequests then
                do hasPendingEvaluationRequests <- false
                do eval ()
        do 
            match isEvaluating with
            | true -> hasPendingEvaluationRequests <- true
            | false -> eval ()
    let ctx = ctxCtor evaluate

    member _.CurrentState = currentState
    member _.IsEvaluating = isEvaluating
    member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
    member _.EvaluationCount = evaluationCount
    member _.RootContext = ctx
    member _.RequestEvaluation() = ctx.RequestEvaluation()

module App =
    let create content ctxCtor onEvaluated =
        VideApp(content, ctxCtor, onEvaluated)
    let createWithObjState content ctxCtor onEvaluated =
        let content =
            Vide <| fun (s: obj option) ctx ->
                let (Vide content) = content
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        create content ctxCtor onEvaluated
