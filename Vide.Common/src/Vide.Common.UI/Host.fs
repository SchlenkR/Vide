namespace Vide

type HostContext<'ctx> = { host: IHost; ctx: 'ctx }

and IHost =
    abstract member RequestEvaluation: unit -> unit
    abstract member SuspendEvaluation: unit -> unit
    abstract member ResumeEvaluation: unit -> unit
    abstract member IsEvaluating: bool
    abstract member HasPendingEvaluationRequests: bool
    abstract member EvaluationCount: uint64
