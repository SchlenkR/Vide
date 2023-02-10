[<AutoOpen>]
module Vide.Fable

open System
open System.Runtime.CompilerServices
open Vide

type TextNode<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

type WebDocument<'n>
    (
        appendChildToParent, 
        removeChildFromParent,
        createElementByName,
        createText,
        getChildNodes,
        clearParentContent
    ) =
    member _.CreateElementByName(name: string) : 'n =
        createElementByName(name)
    member _.CreateText(text: string) : TextNode<'n> =
        createText(text)
    member _.AppendChildToParent(parent: 'n, child: 'n) : unit =
        appendChildToParent(parent, child)
    member _.RemoveChildFromParent(parent: 'n, child: 'n) : unit =
        removeChildFromParent(parent, child)
    member _.GetChildNodes(parent: 'n) : 'n list =
        getChildNodes(parent)
    member _.ClearContent(parent: 'n) : unit =
        clearParentContent(parent)

[<AbstractClass>]
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        evaluationManager: IEvaluationManager,
        document: WebDocument<'n>
    ) =
    let mutable keptChildren = []

    let keepChild child = keptChildren <- child :: keptChildren
    let appendToParent child = document.AppendChildToParent(parent, child)
    let removeFromParent child = document.RemoveChildFromParent(parent, child)

    interface IVideContext with
        member _.EvaluationManager = evaluationManager
    member _.EvaluationManager = evaluationManager
    member _.Parent = parent
    member _.AddElement<'e>(tagName: string) : 'e =
        let n = document.CreateElementByName(tagName)
        do n |> keepChild 
        do n |> appendToParent 
        // TODO: why unsafe cast?
        (box n) :?> 'e
    member _.AddText(text: string) =
        let t = document.CreateText(text)
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
type ChildAction = 
    | Keep
    | DiscardAndCreateNew

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
    let inline apply<^v1, ^v2, ^fs, ^n, ^nc, ^c
                        when ^nc: equality
                        and ^c :> NodeContext<^nc>
                        and ^c: (member CreateChildCtx: ^nc -> ^c)
                        >
        (Vide childVide: Vide<^v1,^fs,^c>)
        (createResultVal: ^n -> ^v1 -> ^v2)
        (modifierCtx: ModifierContext<^n,^c>)
        : Vide<'v2, NodeBuilderState<^n,^fs>, ^c>
        =
        Vide <| fun s (ctx: ^c) ->
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
                        ctx.KeepChild((box node) :?> ^nc)
                        node,cs
                    | DiscardAndCreateNew ->
                        modifierCtx.CreateNode ctx,None
            do runModifiers modifierCtx.EvalModifiers node
            let childCtx = ctx.CreateChildCtx((box node) :?> ^nc)
            let cv,cs = childVide cs childCtx
            for x in childCtx.GetObsoleteChildren() do
                childCtx.RemoveChild(x)
            do runModifiers modifierCtx.AfterEvalModifiers node
            let result = createResultVal node cv
            let state = Some (Some node, cs)
            result,state

type ComponentRetCnBuilder() =
    inherit VideBaseBuilder()
    member _.Return(x) = BuilderBricks.return'(x)

[<AbstractClass>]
type RenderBaseBuilder<'n,'c>(createNode, checkOrUpdateNode) =
    inherit VideBaseBuilder()
    let modifierContext = ModifierContext<'n,'c>(createNode, checkOrUpdateNode)
    member _.ModifierContext = modifierContext

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

module BuilderBricks =
    let createNode elemName (ctx: NodeContext<_>) =
        ctx.AddElement<'n>(elemName)
    
    let checkOrUpdateNode expectedNodeName (actualNodeName: string) =
        match actualNodeName.Equals(expectedNodeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
    
    let inline yieldText(value: string) =
        Vide <| fun s (ctx: NodeContext<_>) ->
            let textNode = s |> Option.defaultWith (fun () -> ctx.AddText(value))
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.KeepChild(textNode.node)
            (), Some textNode
    
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp(op: BuilderOperations) =
        Vide <| fun s (ctx: NodeContext<_>) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None








open Browser
open Browser.Types

type FableContext
    (
        parent: Node, 
        evaluationManager: IEvaluationManager
    ) =
    inherit NodeContext<Node>
        (
            parent,
            evaluationManager,
            WebDocument(
                (fun (parent,child) -> parent.appendChild(child) |> ignore),
                (fun (parent,child) -> parent.removeChild(child) |> ignore),
                (fun tagName -> document.createElement tagName),
                (fun text ->
                    let tn = document.createTextNode(text)
                    let textNode =
                        {
                            node = tn :> Node
                            getText = fun () -> tn.textContent
                            setText = fun value -> tn.textContent <- value
                        }
                    textNode
                ),
                (fun parent -> 
                    let nodes = parent.childNodes
                    [ for i in 0 .. nodes.length-1 do nodes.Item i ]
                ),
                (fun parent -> parent.textContent <- "")
            )
        )
    // SRTP resolved
    member _.CreateChildCtx(parent) = FableContext(parent, evaluationManager)

// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for renderers (used for HTML elements like
//    div, p, etc.) in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

// -------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type RenderValC0Builder<'v,'n when 'n :> Node and 'n: equality>(createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) =
    inherit RenderBaseBuilder<'n, FableContext>(createNode, checkOrUpdateNode)
    member this.Run<'v1,'fs>(v) =
        this.ModifierContext |> ModifierContext.apply<'v1,'v,'fs,'n,Node,FableContext> v (fun n v -> createResultVal n)

type RenderRetC0Builder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderBaseBuilder<'n, FableContext>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)

type RenderValCnBuilder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal: 'n -> 'v) =
    inherit RenderBaseBuilder<'n, FableContext>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetCnBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderBaseBuilder<'n, FableContext>(createNode, checkOrUpdateNode)
    member this.Run(v) = this.ModifierContext |> ModifierContext.apply v (fun n v -> v)
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

module Vide =

    [<GeneralizableValue>]
    let fableContext : Vide<FableContext,unit,FableContext> =
        Vide <| fun s ctx -> ctx,None

    [<GeneralizableValue>]
    let node<'n when 'n :> Node> : Vide<'n, unit, FableContext> =
        Vide <| fun s ctx ->
            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
            ctx.Parent :?> 'n,None

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
