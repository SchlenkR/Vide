[<AutoOpen>]
module Vide.DocumentModel

open System
open System.Runtime.CompilerServices
open Vide

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

type INodeDocument<'n> =
    abstract member CreateNodeByName : tagName: string -> 'n
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>
    abstract member AppendChild : parent: 'n * child: 'n -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildNodes : parent: 'n -> 'n list
    abstract member ClearContent : parent: 'n -> unit

type INodeContextFactory<'c,'nc 
        when 'nc: equality
        and 'c :> NodeContext<'nc> 
    > =
    abstract member CreateChildCtx : 'nc -> 'c

and [<AbstractClass>] NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        evaluationManager: IEvaluationManager,
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []

    let keepChild child = keptChildren <- child :: keptChildren
    let appendToParent child = document.AppendChild(parent, child)
    let removeFromParent child = document.RemoveChild(parent, child)

    interface IVideContext with
        member _.EvaluationManager = evaluationManager
    member _.EvaluationManager = evaluationManager
    member _.Parent = parent
    member _.AddNode<'e>(tagName: string) : 'e =
        let n = document.CreateNodeByName(tagName)
        do n |> keepChild
        do n |> appendToParent 
        // TODO: Can we get rid of the unsafe cast?
        (box n) :?> 'e
    member _.AddTextNode(text: string) =
        let t = document.CreateTextNode(text)
        do t.node |> keepChild
        do t.node |> appendToParent
        t
    member _.KeepChild(child) =
        child |> keepChild
    member _.RemoveChild(child) =
        removeFromParent(child)
    member _.GetObsoleteChildren() =
        let childNodes = document.GetChildNodes(parent)
        childNodes |> List.except keptChildren
    member _.ClearContent() =
        document.ClearContent(parent)

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
    let inline apply<'v1,'v2,'s,'n,'nc,'c
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
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
            for x in childCtx.GetObsoleteChildren() do
                childCtx.RemoveChild(x)
            do runModifiers modifierCtx.AfterEvalModifiers node
            let result = createResultVal node cv
            let state = Some (Some node, cs)
            result,state

module BuilderBricks =
    let createNode elemName (ctx: #NodeContext<_>) =
        ctx.AddNode<'n>(elemName)
    
    let checkOrUpdateNode expectedNodeName (actualNodeName: string) =
        match actualNodeName.Equals(expectedNodeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
    
    let inline yieldText<'nc,'c when 'c :> NodeContext<'nc>>(value: string) =
        Vide <| fun s (ctx: 'c) ->
            let textNode = s |> Option.defaultWith (fun () -> ctx.AddTextNode(value))
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.KeepChild(textNode.node)
            (), Some textNode
    
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp(op: BuilderOperations) =
        Vide <| fun s (ctx: #NodeContext<_>) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None

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
// -------------------------------------------------------------------

type ComponentRetCnBuilder<'nc,'c when 'c :> NodeContext<'nc> and 'nc : equality>() =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    member _.Delay(f) = BuilderBricks.delay(f)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)

[<AbstractClass>]
type RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode) =
    inherit VideBaseBuilder()
    let modifierContext = ModifierContext<'n,'c>(createNode, checkOrUpdateNode)
    member _.ModifierContext = modifierContext

type RenderValC0BaseBuilder<'v,'c,'n,'nc
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member this.Run<'v1,'s>(v: Vide<'v1,'s,'c>) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC0BaseBuilder<'c,'n,'nc
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> v)

type RenderValC1BaseBuilder<'v,'c,'n,'nc
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member this.Run<'v1,'s>(v: Vide<'v1,'s,'c>) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC1BaseBuilder<'c,'n,'nc
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> v)

type RenderValCnBaseBuilder<'v,'c,'n,'nc
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) =
        this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetCnBaseBuilder<'c,'n,'nc
            when 'nc: equality
            and 'c :> NodeContext<'nc>
            and 'c :> INodeContextFactory<'c,'nc>
    >
    (createNode, checkOrUpdateNode) 
    =
    inherit RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)



// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type RenderRetC1BaseBuilder<'c,'n,'nc
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'c,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder<_,_>) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type RenderRetCnBaseBuilder<'c,'n,'nc
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'c,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder<_,_>) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type ComponentRetCnBuilder<'nc,'c
        when 'c :> NodeContext<'nc> 
        and 'nc : equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder<_,_>) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText<'nc,'c> s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'c,'n,'nc
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'c,'nc>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'c,'n,'nc
        when 'nc: equality
        and 'c :> NodeContext<'nc>
        and 'c :> INodeContextFactory<'c,'nc>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBuilder<'nc,'c
        when 'c :> NodeContext<'nc> 
        and 'nc : equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

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
