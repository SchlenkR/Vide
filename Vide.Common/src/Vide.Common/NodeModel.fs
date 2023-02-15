[<AutoOpen>]
module Vide.NodeModel

open System.Runtime.CompilerServices
open Vide

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

type INodeDocument<'n> =
    abstract member AppendChild : parent: 'n * child: 'n -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildNodes : parent: 'n -> 'n list
    abstract member ClearContent : parent: 'n -> unit
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

[<AbstractClass>] 
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        evaluationManager: IEvaluationManager,
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []

    interface IVideContext with
        member _.EvaluationManager = evaluationManager
    member _.EvaluationManager = evaluationManager
    member _.Parent = parent
    member _.KeepChild(child) =
        do keptChildren <- child :: keptChildren
    member _.RemoveObsoleteChildren() =
        do 
            document.GetChildNodes(parent)
            |> List.except keptChildren
            |> List.iter (fun child -> document.RemoveChild(parent, child))
    member _.ClearContent() =
        do document.ClearContent(parent)
    member this.AddTextNode(text: string) =
        let t = document.CreateTextNode(text)
        do this.KeepChild(t.node)
        do document.AppendChild(parent, t.node)
        t

type INodeContextFactory<'nc,'c when 'nc: equality and 'c :> NodeContext<'nc>> =
    abstract member CreateChildCtx : 'nc -> 'c

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
        checkNode: 'n -> ChildAction
    ) =

    member _.CreateNode = createNode
    member _.CheckOrUpdateNode = checkNode

    member val InitModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'n,'c>> = ResizeArray() with get

module ModifierContext =
    let inline apply<'v1,'v2,'s,'n,'nc,'c
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'nc,'c>
        >
        (Vide childVide: Vide<'v1,'s,'c>)
        (createResultVal: 'n -> 'v1 -> 'v2)
        (modifierCtx: ModifierContext<'n,'c>)
        : Vide<'v2, NodeBuilderState<'n,'s>, 'c>
        =
        Vide <| fun s (ctx: 'c) ->
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
                    let newNode,s = modifierCtx.CreateNode(ctx), cs
                    do runModifiers modifierCtx.InitModifiers newNode
                    newNode,s
                | Some node ->
                    match modifierCtx.CheckOrUpdateNode(node) with
                    | Keep ->
                        ctx.KeepChild((box node) :?> 'nc)
                        node,cs
                    | DiscardAndCreateNew ->
                        modifierCtx.CreateNode ctx,None
            do runModifiers modifierCtx.EvalModifiers node
            let childCtx =
                // TODO: Why the unsafe cast everywhere in this function?
                ctx.CreateChildCtx((box node) :?> 'nc)
            let cv,cs = childVide cs childCtx
            do childCtx.RemoveObsoleteChildren()
            do runModifiers modifierCtx.AfterEvalModifiers node
            let result = createResultVal node cv
            let state = Some (Some node, cs)
            result,state

module BuilderBricks =
   
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp(op: BuilderOperations) =
        Vide <| fun s (ctx: #NodeContext<_>) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None
    
    let inline yieldText<'nc,'c when 'c :> NodeContext<'nc>>(value: string) =
        Vide <| fun s (ctx: 'c) ->
            let textNode = s |> Option.defaultWith (fun () -> ctx.AddTextNode(value))
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.KeepChild(textNode.node)
            (), Some textNode


// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for "vide { .. }" and renderers
// Used for HTML elements like
//    div, p, etc.in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

// -------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
//   + Combine,For,Delay
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'nc,'c
        when 'nc : equality
        and 'c :> NodeContext<'nc> 
    > () =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    member _.Delay(f) = BuilderBricks.delay(f)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)

[<AbstractClass>]
type RenderBaseBuilder<'n,'c>(createNode, checkNode) =
    inherit VideBaseBuilder()
    let modifierContext = ModifierContext<'n,'c>(createNode, checkNode)
    member _.ModifierContext = modifierContext

type RenderValC0BaseBuilder<'v,'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member this.Run<'v1,'s>(v: Vide<'v1,'s,'c>) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC0BaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> v)

type RenderValC1BaseBuilder<'v,'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member this.Run<'v1,'s>(v: Vide<'v1,'s,'c>) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC1BaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> v)

type RenderValCnBaseBuilder<'v,'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetCnBaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    >
    (createNode, checkNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)



// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type RenderRetC1BaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type RenderRetCnBaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type ComponentRetCnBaseBuilder<'nc,'c
        when 'c :> NodeContext<'nc> 
        and 'nc : equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'n,'nc,'c
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'nc,'c>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'nc,'c
        when 'c :> NodeContext<'nc> 
        and 'nc : equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member OnInit(this: #RenderBaseBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnEval(this: #RenderBaseBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.EvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnAfterEval(this: #RenderBaseBuilder<_,_>, m: NodeModifier<_,_>) =
        do this.ModifierContext.AfterEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'n,'c> =
        {
            node: 'n
            evt: 'evt
            ctx: 'c
            mutable requestEvaluation: bool
        }
    
    let inline handle<'n,'nc,'c,'evt when 'c :> NodeContext<'nc>>
        (node: 'n) 
        (ctx: 'c) 
        (callback: NodeEventArgs<'evt,'n,'c> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; ctx = ctx; requestEvaluation = true }
            try
                do ctx.EvaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    ctx.EvaluationManager.RequestEvaluation()
            finally
                do ctx.EvaluationManager.Resume()
