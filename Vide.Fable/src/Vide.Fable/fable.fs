[<AutoOpen>]
module Vide.Fable

open System
open System.Runtime.CompilerServices
open Browser
open Browser.Types
open Vide

type FableContext
    (
        parent: Node, 
        evaluationManager: IEvaluationManager
    ) =
    let mutable keptChildren = []

    let keepChild child = keptChildren <- (child :> Node) :: keptChildren
    let appendToParent child = parent.appendChild(child) |> ignore
    let removeFromParent child = parent.removeChild(child) |> ignore

    interface IVideContext with
        member _.EvaluationManager = evaluationManager
    member _.EvaluationManager = evaluationManager
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

type NodeModifierContext<'n,'c> =
    {
        node: 'n
        context: 'c
    }
type NodeModifier<'n,'c> = NodeModifierContext<'n,'c> -> unit

type ModifierContext<'n,'c>
    (
        createNode: 'c -> 'n,
        checkOrUpdateNode: 'n -> ChildAction
    ) =

    member _.CreateNode = createNode
    member _.CheckOrUpdateNode = checkOrUpdateNode

    member val InitModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get

module ModifierContext =
    let apply
        (Vide childVide: Vide<'v1,'fs,FableContext>)
        createResultVal
        (modifierCtx: ModifierContext<'n,FableContext>)
        : Vide<'v2, NodeBuilderState<'n,'fs>, FableContext>
        =
        Vide <| fun s (ctx: FableContext) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; context = ctx }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let node,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newNode,s = modifierCtx.CreateNode ctx,cs
                    do runModifiers modifierCtx.InitModifiers newNode
                    newNode,s
                | Some node ->
                    match modifierCtx.CheckOrUpdateNode node with
                    | Keep ->
                        ctx.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        modifierCtx.CreateNode ctx,None
            do runModifiers modifierCtx.EvalModifiers node
            let childCtx = FableContext(node, ctx.EvaluationManager)
            let cv,cs = childVide cs childCtx
            for x in childCtx.GetObsoleteChildren() do
                childCtx.RemoveChild(x)
            do runModifiers modifierCtx.AfterEvalModifiers node
            let result = createResultVal node cv
            let state = Some (Some node, cs)
            result,state

type IRenderBuilder<'n,'c> =
    abstract member ModifierContext: ModifierContext<'n,'c> with get

module BuilderBricks =
    let createNode elemName (ctx: FableContext) =
        ctx.AddElement<'n>(elemName)
    
    let checkOrUpdateNode elemName (node: #Node) =
        match node.nodeName.Equals(elemName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO:
            console.log($"TODO: if/else detection? Expected node name: {elemName}, but was: {node.nodeName}")
            DiscardAndCreateNew
    
    let inline yieldText(value: string) =
        Vide <| fun s (ctx: FableContext) ->
            let node = s |> Option.defaultWith (fun () -> ctx.AddText(value))
            do
                if node.textContent <> value then
                    node.textContent <- value
                ctx.KeepChild(node)
            (), Some node
    
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp(op: BuilderOperations) =
        Vide <| fun s (ctx: FableContext) ->
            match op with
            | Clear -> ctx.Parent.textContent <- ""
            (),None

// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for renderers (used for HTML elements like
//    div, p, etc.) in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

[<AbstractClass>]
type RenderBaseBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit VideBaseBuilder()
    let modifierContext = ModifierContext(createNode, checkOrUpdateNode)
    interface IRenderBuilder<'n, FableContext> with
        member _.ModifierContext = modifierContext
    member _.ModifierContext = modifierContext

// -------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type RenderValC0Builder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) =
    inherit RenderBaseBuilder<'n>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC0Builder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderBaseBuilder<'n>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)

type RenderValCnBuilder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) =
    inherit RenderBaseBuilder<'n>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetCnBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderBaseBuilder<'n>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)

type ComponentRetCnBuilder() =
    inherit VideBaseBuilder()
    member _.Return(x) = BuilderBricks.return'(x)

// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type RenderRetCnBuilder<'n when 'n :> Node> with
    member _.Yield(b: RenderValC0Builder<_,_>) = b {()}
    member _.Yield(b: RenderRetC0Builder<_>) = b {()}
    member _.Yield(b: RenderRetCnBuilder<_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type ComponentRetCnBuilder with
    member _.Yield(b: RenderValC0Builder<_,_>) = b {()}
    member _.Yield(b: RenderRetC0Builder<_>) = b {()}
    member _.Yield(b: RenderRetCnBuilder<_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetCnBuilder<'n when 'n :> Node> with
    member _.Bind(m: RenderValC0Builder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0Builder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBuilder with
    member _.Bind(m: RenderValC0Builder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0Builder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member OnInit(this: #IRenderBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnEval(this: #IRenderBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.EvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnAfterEval(this: #IRenderBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.AfterEvalModifiers.Add(m)
        this

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

//module Vide =
//    [<GeneralizableValue>]
//    let node<'n when 'n :> Node> : Vide<_, NodeBuilderState<'n,'s>, _> =
//        Vide <| fun s ctx ->
//            // TODO: OUCH! 
//            ctx.Parent,None

module VideApp =
    let inline doCreate appCtor (host: #Node) (content: Vide<'v,'s,FableContext>) onEvaluated =
        let content = 
            // TODO: Really a ContentBuilder? Why?
            RenderRetCnBuilder((fun _ -> host), fun _ -> Keep) { content }
        let ctxCtor = fun eval -> FableContext(host, eval)
        appCtor content ctxCtor onEvaluated
    let createFable host content onEvaluated =
        doCreate VideApp.create host content onEvaluated
    let createFableWithObjState host content onEvaluated =
        doCreate VideApp.createWithUntypedState host content onEvaluated

let vide = ComponentRetCnBuilder()
