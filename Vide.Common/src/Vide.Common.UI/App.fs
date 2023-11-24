namespace Vide

open System

type OnEvalCallbackArgs<'v,'s> =
    { evaluationCount: uint64
      value: 'v
      currentState: 's option
      duration: TimeSpan }

type VideApp<'v,'s,'c>
    (
        content: Vide<'v,'s,HostContext<'c>>,
        ctxCtor: unit -> 'c,
        ctxFin: HostContext<'c> -> unit
    ) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
    let mutable onEvaluated: (OnEvalCallbackArgs<'v,'s> -> unit) option = None

    // TODO: rename to IHost or IRoot
    interface IHost with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
                let rec eval () =
                    let startTime = DateTime.Now
                    do
                        hasPendingEvaluationRequests <- false
                        isEvaluating <- true
                    let value,newState = 
                        let ctx = { host = this; ctx = ctxCtor () }
                        let res = (runVide content) currentState ctx
                        do ctxFin ctx
                        res
                    do
                        currValue <- Some value
                        currentState <- newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
                    do
                        let diag = 
                            { 
                                evaluationCount = evaluationCount
                                value = value
                                currentState = currentState
                                duration = DateTime.Now - startTime
                            }
                        onEvaluated |> Option.iter (fun callback -> callback diag)
                    if hasPendingEvaluationRequests then
                        eval ()
                do
                    match isEvaluating with
                    | true -> hasPendingEvaluationRequests <- true
                    | false -> eval ()
        member _.SuspendEvaluation() =
            do suspendEvaluation <- true
        member this.ResumeEvaluation() =
            do suspendEvaluation <- false
            if hasPendingEvaluationRequests then
                (this :> IHost).RequestEvaluation()
        member _.IsEvaluating = isEvaluating
        member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
        member _.EvaluationCount = evaluationCount

    member this.EvaluationManager = this :> IHost
    member _.CurrentState = currentState
    member this.ForceState(state) =
        do currentState <- Some state
        this.EvaluationManager.RequestEvaluation()
    member _.OnEvaluated(evaluationCallback) = onEvaluated <- Some evaluationCallback
    member _.OnEvaluated() = onEvaluated <- None

type VideAppFactory<'c>(ctxCtor, ctxFin) =
    let start (app: VideApp<_,_,'c>) =
        do app.EvaluationManager.RequestEvaluation()
        app
    member _.Create(content) : VideApp<_,_,'c> =
        VideApp(content, ctxCtor, ctxFin)
    member this.CreateWithUntypedState(content: Vide<_,_,_>) : VideApp<_,_,'c> =
        let content =
            mkVide <| fun (s: obj option) ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = (runVide content) typedS ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        this.Create(content)
    member this.CreateAndStart(content) : VideApp<_,_,'c> =
        this.Create(content) |> start
    member this.CreateAndStartWithUntypedState(content) : VideApp<_,_,'c> =
        this.CreateWithUntypedState(content) |> start
