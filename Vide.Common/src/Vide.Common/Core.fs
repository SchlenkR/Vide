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
// -> TODO: Is that still valid, now that we explicitly use preserve/discard?
type Vide<'v,'s,'c> = 's option -> GlobalContext -> 'c -> 'v * 's option

[<AutoOpen>]
module VideMake =
    let ensureVide f : Vide<_,_,_> = f

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

type VideApp<'v,'s,'c>(content: Vide<'v,'s,'c>, ctxCtor: unit -> 'c) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
    let mutable onEvaluated: (VideApp<'v,'s,'c> -> 'v -> 's option -> unit) option = None
        
    let ctx = ctxCtor()

    interface IEvaluationManager with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
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
                    do onEvaluated |> Option.iter (fun x -> x this value currentState)
                    if hasPendingEvaluationRequests then
                        eval ()
                do
                    match isEvaluating with
                    | true -> hasPendingEvaluationRequests <- true
                    | false -> eval ()
        member _.Suspend() =
            do suspendEvaluation <- true
        member this.Resume() =
            do suspendEvaluation <- false
            if hasPendingEvaluationRequests then
                (this :> IEvaluationManager).RequestEvaluation()
        member _.IsEvaluating = isEvaluating
        member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
        member _.EvaluationCount = evaluationCount

    member _.RootContext = ctx
    member this.EvaluationManager = this :> IEvaluationManager
    member _.CurrentState = currentState
    member _.OnEvaluated
        with get() = onEvaluated
        and set(value) = onEvaluated <- value

module VideApp =
    let create content ctxCtor = VideApp(content, ctxCtor)

    let createWithUntypedState (content: Vide<_,_,_>) ctxCtor =
        let content =
            ensureVide <| fun (s: obj option) gc ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        create content ctxCtor

    let createAndStart content ctxCtor =
        let app = VideApp(content, ctxCtor)
        do app.EvaluationManager.RequestEvaluation()
        app

    let createAndStartWithUntypedState (content: Vide<_,_,_>) ctxCtor =
        let content =
            ensureVide <| fun (s: obj option) gc ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS gc ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        createAndStart content ctxCtor

module Vide =

    // Preserves the first value given and discards subsequent values.
    let preserveValue x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultValue x
            s, Some s
    
    let preserveWith x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultWith x
            s, Some s
    
    // TODO: Think about which function is "global" and module-bound
    let map (proj: 'v1 -> 'v2) (v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
        fun s gc ctx ->
            let v,s = v s gc ctx
            proj v, s
    
    // why 's and not unit? -> see comment in "VideBuilder.Zero"
    [<GeneralizableValue>]
    let zero<'c> : Vide<unit,unit,'c> =
        fun s gc ctx -> (),None

    // this "zero", but the form where state is variable
    // -> see comment in "VideBuilder.Zero"
    [<GeneralizableValue>]
    let empty<'s,'c> : Vide<unit,'s,'c> =
        fun s gc ctx -> (),None

    [<GeneralizableValue>]
    let context<'c> : Vide<'c,unit,'c> =
        fun s gc ctx -> ctx,None

    // TODO: Move to keywords? / rename to useState?
    let ofMutable x =
        ensureVide <| fun s gc ctx ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, gc.evaluationManager))
            s, Some s

module BuilderBricks =
    let bind<'v1,'s1,'v2,'s2,'c>
        (
            m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        fun s gc ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let mv,ms = m ms gc ctx
            let f = f mv
            let fv,fs = f fs gc ctx
            fv, Some (ms,fs)

    let return'<'v,'c>
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        fun s gc ctx -> x,None

    let yield'<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        v

    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // We cannot have both, which means: We cannot have "if"s without "else".
    // This is ok (and not unfortunate), because the developer has to make a
    // decision about what should happen: "elseForget" or "elsePreserve".
    let zero<'c>
        ()
        : Vide<unit,unit,'c>
        = Vide.zero<'c>

    let delay<'v,'s,'c>
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
    let combine<'v1,'s1,'v2,'s2,'c>
        (
           a: Vide<'v1,'s1,'c>,
           b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        fun s gc ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let va,sa = a sa gc ctx
            let vb,sb = b sb gc ctx
            vb, Some (sa,sb)

    let for'<'a,'v,'s,'c when 'a: comparison>
        (
            input: seq<'a>,
            body: 'a -> Vide<'v,'s,'c>
        ) 
        : Vide<'v list, Map<'a, 's option>,'c>
        = 
        fun s gc ctx ->
            Debug.print 0 "FOR"
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for x in input do
                    let matchingState = currMap |> Map.tryFind x |> Option.flatten
                    let v,s = 
                        let v = body x
                        v matchingState gc ctx
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
                a: Vide<'v,'s1,'c>,
                b: AsyncBindResult<'x, Vide<'v,'s2,'c>>
            )
            : Vide<'v, 's1 option * AsyncState<_> option * 's2 option, 'c>
            =
            fun s gc ctx ->
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
                            let b = f v
                            let vb,sb = b sb gc ctx
                            vb,comp,sb
                        | None ->
                            va,comp,sb
                v, Some (sa, Some comp, sb)

// TODO:
//    For value restriction and other resolution issues, it's better to
//    move these (remaining) builder methods "as close as possible" to the builder class that's
//    most specialized.
type VideBaseBuilder() =
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()

    // ---------------------
    // ASYNC
    // ---------------------
    member _.Bind(m, f) = BuilderBricks.Async.bind(m, f)
    member _.Delay(f) = BuilderBricks.Async.delay(f)
    member _.Combine(a, b) = BuilderBricks.Async.combine(a, b)
    // TODO: async for

[<AutoOpen>]
module Keywords =
    
    [<GeneralizableValue>]
    let elsePreserve<'s,'c> : Vide<unit,'s,'c> =
        fun s gc ctx -> (),s

    [<GeneralizableValue>]
    let elseForget<'s,'c> : Vide<unit,'s,'c> = 
        Vide.empty<'s,'c>

    type Switch<'v,'s,'c,'g> = Switch of guard:('g -> bool) * caseMatched:bool * view:Vide<'v,'s,'c>
    type [<RequireQualifiedAccess>] CaseType = Always | First

    let switch guard = Switch(guard,false,Vide.zero)

    // The case functions need to be defined in the context of the
    // Vide model, because a generic "vide" builder is not present
    // and it's behaviour is specific.
