namespace Vide

type IEvaluationManager =
    abstract member RequestEvaluation: unit -> unit
    abstract member Suspend: unit -> unit
    abstract member Resume: unit -> unit
    abstract member IsEvaluating: bool
    abstract member HasPendingEvaluationRequests: bool
    abstract member EvaluationCount: uint64

type GlobalContext = { evaluationManager: IEvaluationManager }

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> GlobalContext -> 'c -> 'v * 's option)

module Debug =
    let mutable enabledDebugChannels : int list = []
    let print channel s = if enabledDebugChannels |> List.contains channel then printfn "%s" s

[<AutoOpen>]
module MutableValue =
    type MutableValue<'a when 'a: equality>(initial: 'a, evalManager: IEvaluationManager) =
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

type AsyncState<'v> =
    {
        startedWorker: Async<'v>
        result: Ref<'v option>
    }

type AsyncBindResult<'v1,'v2> = 
    AsyncBindResult of comp: Async<'v1> * cont: ('v1 -> 'v2)

type VideApp<'v,'s,'c>
    (
        content: Vide<'v,'s,'c>,
        ctxCtor: unit -> 'c,
        onEvaluated: VideApp<'v,'s,'c> -> 'v -> 's option -> unit
    ) as this 
    =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
        
    let ctx = ctxCtor()

    interface IEvaluationManager with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
                let (Vide content) = content
                let rec eval () =
                    do 
                        hasPendingEvaluationRequests <- false
                        isEvaluating <- true
                    let value,newState = 
                        let gc = { evaluationManager = this.EvaluationManager } 
                        content currentState gc this.RootContext
                    do
                        currValue <- Some value
                        currentState <- newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
                    do onEvaluated this value currentState
                    if hasPendingEvaluationRequests then
                        eval ()
                do
                    match isEvaluating with
                    | true -> hasPendingEvaluationRequests <- true
                    | false -> eval ()
        member _.Suspend() =
            do suspendEvaluation <- true
        member _.Resume() =
            do suspendEvaluation <- false
            if hasPendingEvaluationRequests then
                (this :> IEvaluationManager).RequestEvaluation()
        member _.IsEvaluating = isEvaluating
        member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
        member _.EvaluationCount = evaluationCount

    member _.RootContext = ctx
    member _.EvaluationManager = this :> IEvaluationManager
    member _.CurrentState = currentState

module VideApp =
    let create content ctxCtor onEvaluated =
        VideApp(content, ctxCtor, onEvaluated)
    let createWithUntypedState content ctxCtor onEvaluated =
        let content =
            Vide <| fun (s: obj option) gc ctx ->
                let (Vide content) = content
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        create content ctxCtor onEvaluated
    let createAndStart content ctxCtor onEvaluated =
        let app = VideApp(content, ctxCtor, onEvaluated)
        do app.EvaluationManager.RequestEvaluation()
        app
    let createAndStartWithUntypedState content ctxCtor onEvaluated =
        let content =
            Vide <| fun (s: obj option) gc ctx ->
                let (Vide content) = content
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        createAndStart content ctxCtor onEvaluated

module Vide =

    // Preserves the first value given and discards subsequent values.
    let preserveValue x =
        Vide <| fun s gc ctx ->
            let s = s |> Option.defaultValue x
            s, Some s
    
    let preserveWith x =
        Vide <| fun s gc ctx ->
            let s = s |> Option.defaultWith x
            s, Some s
    
    // TODO: Think about which function is "global" and module-bound
    let map (proj: 'v1 -> 'v2) (Vide v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
        Vide <| fun s gc ctx ->
            let v,s = v s gc ctx
            proj v, s
    
    // why 's and not unit? -> see comment in "VideBuilder.Zero"
    [<GeneralizableValue>]
    let zero<'s,'c> : Vide<unit,'s,'c> =
        Vide <| fun s gc ctx -> (),None
    
    [<GeneralizableValue>]
    let context<'c> : Vide<'c,unit,'c> =
        Vide <| fun s gc ctx -> ctx,None

    let ofMutable x =
        Vide <| fun s gc ctx ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, gc.evaluationManager))
            s, Some s

module BuilderBricks =
    let bind
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s gc ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let mv,ms = m ms gc ctx
            let (Vide f) = f mv
            let fv,fs = f fs gc ctx
            fv, Some (ms,fs)

    let return'<'v,'c>
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        Vide <| fun s gc ctx -> x,None

    let yield'<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        v

    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // Unfortunately, we cannot have both. For that reason, "if"s without "else"
    // must use "else elseZero".
    let zero
        ()
        : Vide<unit,unit,'c>
        = Vide.zero<unit,'c>

    let delay
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        f()

    // TODO
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
    let combine
        (
            Vide a: Vide<'v1,'s1,'c>,
            Vide b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s gc ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let va,sa = a sa gc ctx
            let vb,sb = b sb gc ctx
            vb, Some (sa,sb)

    let for'
        (
            input: seq<'a>,
            body: 'a -> Vide<'v,'s,'c>
        ) 
        : Vide<'v list, Map<'a, 's option>,'c>
        = 
        Vide <| fun s gc ctx ->
            Debug.print 0 "FOR"
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for x in input do
                    let matchingState = currMap |> Map.tryFind x |> Option.flatten
                    let v,s = let (Vide v) = body x in v matchingState gc ctx
                    do currMap <- currMap |> Map.remove x
                    v, (x,s)
                ]
                |> List.unzip
            resValues, Some (resStates |> Map.ofList)
        //: Vide<'v list, list<'s option>, 'c>
        //= 
        //Vide <| fun s ctx ->
        //    let lastStates = s |> Option.defaultValue []
        //    let resValues,resStates =
        //        [ for i,x in input |> Seq.indexed do
        //            let matchingState = lastStates |> List.tryItem i |> Option.flatten
        //            let v,s = let (Vide v) = body x in v matchingState ctx
        //            v,s
        //        ]
        //        |> List.unzip
        //    resValues, Some resStates

    // ---------------------
    // ASYNC
    // ---------------------
    
    module Async =
        let bind<'v1,'v2,'s,'c>
            (
                m: Async<'v1>,
                f: 'v1 -> Vide<'v2,'s,'c>
            ) : AsyncBindResult<'v1, Vide<'v2,'s,'c>>
            =
            AsyncBindResult(m, f)
    
        let delay
            (f: unit -> AsyncBindResult<'v1,'v2>)
            : AsyncBindResult<'v1,'v2>
            =
            f()
    
        let combine<'v,'x,'s1,'s2,'c>
            (
                Vide a: Vide<'v,'s1,'c>,
                b: AsyncBindResult<'x, Vide<'v,'s2,'c>>
            )
            : Vide<'v, 's1 option * AsyncState<_> option * 's2 option, 'c>
            =
            Vide <| fun s gc ctx ->
                let sa,comp,sb =
                    match s with
                    | None -> None,None,None
                    | Some (sa,comp,sb) -> sa,comp,sb
                // TODO: Really reevaluate here at this place?
                let va,sa = a sa gc ctx
                let v,comp,sb =
                    match comp with
                    | None ->
                        let (AsyncBindResult (comp,_)) = b
                        let result = ref None
                        do
                            let onsuccess res =
                                Debug.print 1 $"awaited result: {res}"
                                do result.Value <- Some res
                                do gc.evaluationManager.RequestEvaluation()
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
                            let vb,sb = b sb gc ctx
                            vb,comp,sb
                        | None ->
                            va,comp,sb
                v, Some (sa, Some comp, sb)

[<AbstractClass>]
type VideBaseBuilder() =
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()
    member _.Delay(f) = BuilderBricks.delay(f)

    // ---------------------
    // ASYNC
    // ---------------------
    member _.Bind(m, f) = BuilderBricks.Async.bind(m, f)
    member _.Delay(f) = BuilderBricks.Async.delay(f)
    member _.Combine(a, b) = BuilderBricks.Async.combine(a, b)
    // TODO: async for
