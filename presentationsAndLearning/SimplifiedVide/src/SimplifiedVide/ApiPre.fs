namespace Vide

open System.Runtime.CompilerServices
open Vide

[<Extension>]
type NodeBuilderExtensions =

    /// Called once on initialization.
    [<Extension>]
    static member onInit(this: #NodeBuilder<'e>, m: NodeModifier<'e>) =
        do this.InitModifiers.Add(m)
        this

    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member onEval(this: #NodeBuilder<'e>, m: NodeModifier<'e>) =
        do this.PreEvalModifiers.Add(m)
        this

    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member onAfterEval(this: #NodeBuilder<'e>, m: NodeModifier<'e>) =
        do this.PostEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'e> =
        {
            node: 'e
            evt: 'evt
            app: IApp
            mutable requestEvaluation: bool
        }

    let inline handle (node: 'e) (app: IApp) (callback: NodeEventArgs<'evt,'e> -> unit) =
        fun evt ->
            let args = { node = node; evt = evt; app = app; requestEvaluation = true }
            try
                do app.SuspendEvaluation()
                do callback args
                if args.requestEvaluation then
                    app.RequestEvaluation()
            finally
                do app.ResumeEvaluation()
