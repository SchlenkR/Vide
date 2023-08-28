namespace Vide

type VideApp<'v,'s,'c>(content: Vide<'v,'s,'c>, ctxCtor: unit -> 'c, ctxFin: 'c -> unit) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
    let mutable onEvaluated: ('v -> 's option -> unit) option = None

    // TODO: rename to IHost or IRoot
    interface IHost with
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
                        let ctx = ctxCtor ()
                        let (Vide content) = content
                        let res = content currentState ctx
                        do ctxFin ctx
                        res
                    do
                        currValue <- Some value
                        currentState <- newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
                    do onEvaluated |> Option.iter (fun x -> x value currentState)
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
    member _.OnEvaluated(value) = onEvaluated <- Some value
    member _.OnEvaluated() = onEvaluated <- None

type VideAppFactory<'c>(ctxCtor, ctxFin) =
    let start (app: VideApp<_,_,'c>) =
        do app.EvaluationManager.RequestEvaluation()
        app
    member _.Create(content) : VideApp<_,_,'c> =
        VideApp(content, ctxCtor, ctxFin)
    member this.CreateWithUntypedState(Vide content) : VideApp<_,_,'c> =
        let content =
            Vide <| fun (s: obj option) ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = content typedS ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        this.Create(content)
    member this.CreateAndStart(content) : VideApp<_,_,'c> =
        this.Create(content) |> start
    member this.CreateAndStartWithUntypedState(content) : VideApp<_,_,'c> =
        this.CreateWithUntypedState(content) |> start
