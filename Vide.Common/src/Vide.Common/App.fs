namespace Vide

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
