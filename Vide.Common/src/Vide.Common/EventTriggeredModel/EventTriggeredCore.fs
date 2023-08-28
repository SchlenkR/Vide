namespace Vide

type HostContext<'ctx> = { host: IHost; ctx: 'ctx }

and IHost =
    abstract member RequestEvaluation: unit -> unit
    abstract member SuspendEvaluation: unit -> unit
    abstract member ResumeEvaluation: unit -> unit
    abstract member IsEvaluating: bool
    abstract member HasPendingEvaluationRequests: bool
    abstract member EvaluationCount: uint64

type AsyncState<'v> =
    {
        startedWorker: Async<'v>
        result: Ref<'v option>
    }

type AsyncBindResult<'v1,'v2> = 
    AsyncBindResult of comp: Async<'v1> * cont: ('v1 -> 'v2)

module AsyncBuilderBricks =
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

    let combine<'v, 'x, 's1, 's2, 'c>
        (
            Vide a: Vide<'v, 's1, HostContext<'c>>,
            b: AsyncBindResult<'x, Vide<'v, 's2, HostContext<'c>>>
        )
        : Vide<'v, 's1 option * AsyncState<_> option * 's2 option, HostContext<'c>>
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
                            do result.Value <- Some res
                            do ctx.host.RequestEvaluation()
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

[<AutoOpen>]
module VideBaseBuilderAsyncExtensions =
    type VideBaseBuilder with
        member _.Bind(m, f) = AsyncBuilderBricks.bind(m, f)
        member _.Delay(f) = AsyncBuilderBricks.delay(f)
        member _.Combine(a, b) = AsyncBuilderBricks.combine(a, b)
        // TODO: async for
