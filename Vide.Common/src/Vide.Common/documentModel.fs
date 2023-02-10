[<AutoOpen>]
module Vide.DocumentModel

open System
open System.Runtime.CompilerServices
open Vide

type TextNode<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

[<AbstractClass>]
type WebDocument<'n>() =
    abstract member CreateElementByName : tagName:string -> 'n
    abstract member CreateText : text:string -> TextNode<'n>
    abstract member AppendChildToParent : parent:'n * child:'n -> unit
    abstract member RemoveChildFromParent : parent:'n * child:'n -> unit
    abstract member GetChildNodes : parent:'n -> 'n list
    abstract member ClearContent : parent:'n -> unit

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

type ComponentRetCnBuilder<'c>() =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)

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
    let createNode elemName (ctx: #NodeContext<_>) =
        ctx.AddElement<'n>(elemName)
    
    let checkOrUpdateNode expectedNodeName (actualNodeName: string) =
        match actualNodeName.Equals(expectedNodeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
    
    let inline yieldText<'n,'c when 'c :> NodeContext<'n>>(value: string) =
        Vide <| fun s (ctx: 'c) ->
            let textNode = s |> Option.defaultWith (fun () -> ctx.AddText(value))
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

module Event =
    type FableEventArgs<'evt,'n,'c> =
        {
            node: 'n
            evt: 'evt
            ctx: 'c
            mutable requestEvaluation: bool
        }
    
    let inline handle<'n,'nc,'c,'evt when 'c :> NodeContext<'nc>>
        (node: 'n) 
        (ctx: 'c) 
        (callback: FableEventArgs<'evt,'n,'c> -> unit)
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
