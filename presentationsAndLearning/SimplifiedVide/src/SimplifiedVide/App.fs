namespace Vide

type VideApp<'v,'s>
    (
        content: Vide<'v,'s,NodeContext>,
        ctxCtor: unit -> NodeContext, 
        ctxFin: NodeContext -> unit
    )
    =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false

    interface IApp with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
                let rec eval () =
                    do hasPendingEvaluationRequests <- false
                    do isEvaluating <- true
                    let value,newState = 
                        let ctx = ctxCtor ()
                        let (Vide videContent) = content
                        let res = videContent currentState (this :> IApp) ctx
                        do ctxFin ctx
                        res
                    do
                        currValue <- Some value
                        currentState <- Some newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
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
                (this :> IApp).RequestEvaluation()

module VideApp =
    let createAndStart(host, content) =
        let ctxCtor () = NodeContext(host)
        let ctxFin (ctx: NodeContext) = do ctx.RemoveObsoleteChildren()
        let app = VideApp(content, ctxCtor, ctxFin)
        do (app :> IApp).RequestEvaluation()
        app
