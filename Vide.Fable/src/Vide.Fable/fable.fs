[<AutoOpen>]
module Vide.Fable

open System.Runtime.CompilerServices
open Browser
open Browser.Types
open Vide
open System

type FableContext
    (
        parent: Node, 
        evaluationManager: IEvaluationManager
    ) =
    inherit VideContext()
    
    let mutable keptChildren = []

    let keepChild child = keptChildren <- (child :> Node) :: keptChildren
    let appendToParent child = parent.appendChild(child) |> ignore
    let removeFromParent child = parent.removeChild(child) |> ignore

    override _.EvaluationManager = evaluationManager
    member _.Parent = parent
    member _.AddElement<'n when 'n :> HTMLElement>(tagName: string) =
        let elem = document.createElement tagName 
        do elem |> keepChild 
        do elem |> appendToParent 
        elem :?> 'n
    member _.AddText(text: string) =
        let elem = document.createTextNode text 
        do elem |> keepChild
        do elem |> appendToParent
        elem
    member _.KeepChild(child: Node) =
        child |> keepChild |> ignore
    member _.RemoveChild(child: Node) =
        removeFromParent child
    member _.GetObsoleteChildren() =
        let childNodes =
            let nodes = parent.childNodes
            [ for i in 0 .. nodes.length-1 do nodes.Item i ]
        childNodes |> List.except keptChildren

type BuilderOperations = | Clear

type NodeBuilderState<'n,'s> = option<'n> * option<'s>
type ChildAction = Keep | DiscardAndCreateNew

module BuilderHelper =
    let createNode elemName (ctx: FableContext) =
        ctx.AddElement<'n>(elemName)
    let checkOrUpdateNode elemName (node: #Node) =
        match node.nodeName.Equals(elemName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO:
            console.log($"TODO: if/else detection? Expected node name: {elemName}, but was: {node.nodeName}")
            DiscardAndCreateNew
    let runBuilder
        createNode
        checkOrUpdateNode
        initModifiers
        evalModifiers
        afterEvalModifiers
        (Vide childVide: Vide<'v1,'fs,FableContext>)
        resultSelector
        : Vide<'v2, NodeBuilderState<'n,'fs>, FableContext>
        =
        Vide <| fun s (ctx: FableContext) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m node ctx
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let node,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newNode,s = createNode ctx,cs
                    do runModifiers initModifiers newNode
                    newNode,s
                | Some node ->
                    match checkOrUpdateNode node with
                    | Keep ->
                        ctx.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        createNode ctx,None
            do runModifiers evalModifiers node
            let childCtx = FableContext(node, ctx.EvaluationManager)
            let cv,cs = childVide cs childCtx
            for x in childCtx.GetObsoleteChildren() do
                childCtx.RemoveChild(x)
            do runModifiers afterEvalModifiers node
            let result = resultSelector node cv
            let state = Some (Some node, cs)
            result,state

type NodeModifier<'n,'c> = 'n -> 'c -> unit

type NodeBuilder<'n when 'n :> Node>
    (
        createNode: FableContext -> 'n,
        checkOrUpdateNode: 'n -> ChildAction
    ) =
    inherit VideBuilder()

    member _.CreateNode = createNode
    member _.CheckOrUpdateNode = checkOrUpdateNode

    member val InitModifiers: NodeModifier<'n, FableContext> list = [] with get,set
    member val EvalModifiers: NodeModifier<'n, FableContext> list = [] with get,set
    member val AfterEvalModifiers: NodeModifier<'n, FableContext> list = [] with get,set

    // Important: Run must be defined on this type so that it is visible in upcoming
    // yields - otherwise, Run is not called even though inheritors define it.
    // See Bug: nb {()} - OnEval will not be called

    member this.DoRun(childVide, resultSelector) =
        BuilderHelper.runBuilder
            createNode 
            checkOrUpdateNode 
            this.InitModifiers 
            this.EvalModifiers 
            this.AfterEvalModifiers 
            childVide 
            resultSelector

let inline text value =
    Vide <| fun s (ctx: FableContext) ->
        let node = s |> Option.defaultWith (fun () -> ctx.AddText(value))
        do
            if node.textContent <> value then
                node.textContent <- value
            ctx.KeepChild(node)
        (), Some node

module Yield =
    let inline yieldText (value: string) =
        text value
    let inline yieldBuilderOp (op: BuilderOperations) =
        Vide <| fun s (ctx: FableContext) ->
            match op with
            | Clear -> ctx.Parent.textContent <- ""
            (),None

type VoidBuilder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, resultSelector: 'n -> 'v) =
    inherit NodeBuilder<'n>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.DoRun(v, fun node _ -> resultSelector node)

type ContentBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit NodeBuilder<'n>(createNode, checkOrUpdateNode)
    // Oh, this is a good one: Run must definitely defined before Yield(ContentBuilder)
    member this.Run(v) = this.DoRun(v, fun node vres -> vres)
    member _.Yield(b: VoidBuilder<_,_>) = b {()}
    member _.Yield(b: ContentBuilder<_>) = b {()}
    member _.Yield(s) = Yield.yieldText s
    member _.Yield(op) = Yield.yieldBuilderOp op

type VideComponentBuilder() =
    inherit VideBuilder()
    member _.Yield(b: VoidBuilder<_,_>) = b {()}
    member _.Yield(b: ContentBuilder<_>) = b {()}
    member _.Yield(s) = Yield.yieldText s
    member _.Yield(op) = Yield.yieldBuilderOp op

type VideBuilder with
    member this.Bind(m: VoidBuilder<'v,'n>, f) =
        this.Bind(m {()}, f)

type HTMLVoidElementBuilder<'v,'n when 'n :> HTMLElement>(elemName, resultSelector) =
    inherit VoidBuilder<'v,'n>(
        BuilderHelper.createNode elemName,
        BuilderHelper.checkOrUpdateNode elemName,
        resultSelector)

type HTMLContentElementBuilder<'n when 'n :> HTMLElement>(elemName) =
    inherit ContentBuilder<'n>(
        BuilderHelper.createNode elemName,
        BuilderHelper.checkOrUpdateNode elemName)

[<Extension>]
type NodeBuilderExtensions =
    // TODO: Switch back to non-inheritance API

    /// Called once on initialization.
    [<Extension>]
    static member OnInit(this: #NodeBuilder<_>, m: NodeModifier<_, FableContext>) =
        do this.InitModifiers <- m :: this.InitModifiers
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnEval(this: #NodeBuilder<_>, m: NodeModifier<_, FableContext>) =
        do this.EvalModifiers <- m :: this.EvalModifiers
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnAfterEval(this: #NodeBuilder<_>, m: NodeModifier<_, FableContext>) =
        do this.AfterEvalModifiers <- m :: this.AfterEvalModifiers
        this

type VoidResult = unit

// TODO: All other input possibilities
type InputResult(node: HTMLInputElement) =
    member _.Node = node
    member _.TextValue = node.value
    member _.DateValue = node.valueAsDate
    member _.FloatValue = node.valueAsNumber
    member _.IntValue = node.valueAsNumber |> int
    member _.IsChecked = node.checked

module Event =
    type FableEventArgs<'evt,'n when 'n :> Node> =
        {
            node: 'n
            evt: 'evt
            ctx: FableContext
            mutable requestEvaluation: bool
        }
    
    let inline handle node (ctx: FableContext) callback =
        fun evt ->
            let args = { node = node; evt = evt; ctx = ctx; requestEvaluation = true }
            try
                do ctx.EvaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    ctx.EvaluationManager.RequestEvaluation()
            finally
                do ctx.EvaluationManager.Resume()

type Event =
    static member inline doBind(value: MutableValue<_>, getter) =
        fun (args: Event.FableEventArgs<_, HTMLInputElement>) ->
            value.Value <- getter(InputResult(args.node))
    static member inline bind(value: MutableValue<string>) =
        Event.doBind(value, fun x -> x.TextValue)
    static member inline bind(value: MutableValue<int>) =
        Event.doBind(value, fun x -> x.IntValue)
    static member inline bind(value: MutableValue<float>) =
        Event.doBind(value, fun x -> x.FloatValue)
    static member inline bind(value: MutableValue<DateTime>) =
        Event.doBind(value, fun x -> x.DateValue)
    static member inline bind(value: MutableValue<bool>) =
        Event.doBind(value, fun x -> x.IsChecked)

module App =
    let inline doCreate appCtor (host: #Node) (content: Vide<'v,'s,FableContext>) onEvaluated =
        let content = 
            // TODO: Really a ContentBuilder? Why?
            ContentBuilder((fun _ -> host), fun _ -> Keep) { content }
        let ctxCtor = fun eval -> FableContext(host, eval)
        appCtor content ctxCtor onEvaluated
    let createFable host content onEvaluated =
        doCreate App.create host content onEvaluated
    let createFableWithObjState host content onEvaluated =
        doCreate App.createWithUntypedState host content onEvaluated

let vide = VideComponentBuilder()
