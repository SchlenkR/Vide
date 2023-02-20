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

type NodeCheckResult = Keep | DiscardAndCreateNew

type INodeDocument<'np,'nc> =
    abstract member AppendChild : parent:'np * child:'nc -> unit
    abstract member RemoveChild : parent:'np * child:'nc -> unit
    abstract member GetChildren : parent:'np -> 'nc list
    abstract member ClearChildren : parent:'np -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text:string -> TextNodeProxy<'nc>

type NodeContext<'np,'nc,'d
        when 'np: equality 
        and 'nc: equality  
        and 'd :> INodeDocument<'np,'nc>
    >
    (
        parent: 'np, 
        document: 'd
    ) =
    let mutable keptChildren : 'nc list = []
    member _.Document = document
    member _.KeepChild(child) =
        do keptChildren <- child :: keptChildren
    member this.AppendChild(child) =
        do this.KeepChild(child)
        do document.AppendChild(parent, child)
    member _.RemoveObsoleteChildren() =
        do 
            document.GetChildren(parent)
            |> List.except keptChildren
            |> List.iter (fun child -> document.RemoveChild(parent, child))
    member _.CreateTextNode(value) =
        document.CreateTextNode(value)
    member _.ClearContent() =
        do document.ClearChildren(parent)

type BuilderOperations = | Clear

type NodeBuilderState<'n,'s> = option<'n> * option<'s>

type NodeModifierContext<'n> =
    {
        node: 'n
        globalContext: GlobalContext
    }

type NodeModifier<'n> = NodeModifierContext<'n> -> unit

[<AbstractClass>]
type NodeBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (
        createContext: 'np -> NodeContext<'np,'nc,'d>,
        createThisNode: unit -> 'np,
        checkThisNode: 'np -> NodeCheckResult
    ) =
    
    inherit VideBaseBuilder()

    member val InitModifiers: ResizeArray<NodeModifier<'np>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'np>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'np>> = ResizeArray() with get

    member this.Apply
        (
            Vide childVide: Vide<'v,'s,NodeContext<'np,'nc,_>>,
            createResultVal: 'np -> 'v -> 'vres
        )
        : Vide<'vres,NodeBuilderState<'np,'s>,_>
        =
        Vide <| fun s gc (parentCtx: NodeContext<'npp,'np,_>) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; globalContext = gc }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisNode,cs =
                let createThisNode () =
                    let newNode = createThisNode ()
                    parentCtx.AppendChild(newNode)
                    newNode
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newNode,s = createThisNode(), cs
                    do runModifiers this.InitModifiers newNode
                    newNode,s
                | Some node ->
                    match checkThisNode(node) with
                    | Keep ->
                        parentCtx.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        createThisNode(), None
            do runModifiers this.EvalModifiers thisNode
            let thisCtx = createContext(thisNode)
            let cv,cs = childVide cs gc thisCtx
            do thisCtx.RemoveObsoleteChildren()
            do runModifiers this.AfterEvalModifiers thisNode
            let result = createResultVal thisNode cv
            let state = Some (Some thisNode, cs)
            result,state

module BuilderBricks =
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp (op: BuilderOperations) =
        Vide <| fun s gc (ctx: NodeContext<_,_,_>) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None

    let inline yieldText (value: string) =
        Vide <| fun s gc (ctx: NodeContext<_,_,_>) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.CreateTextNode(value)
                    do ctx.AppendChild(textNode.node)
                    textNode
                )
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

//let combineNodes
//    (
//        Vide a: Vide<'v1,'s1,NodeContext<_>,
//        Vide b: Vide<'v2,'s2,'c>
//    )
//    : Vide<'v2,'s1 option * 's2 option,'c>
//    =
//    Vide <| fun s gc ctx ->
//        let sa,sb =
//            match s with
//            | None -> None,None
//            | Some (ms,fs) -> ms,fs
//        let va,sa = a sa gc ctx
//        let vb,sb = b sb gc ctx
//        vb, Some (sa,sb)

type ComponentRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > () =
    inherit VideBaseBuilder()
    member _.Return(x) = BuilderBricks.return'(x)
    member _.Delay(f) = BuilderBricks.delay(f)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)

type RenderValC0BaseBuilder<'v,'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode, createResultVal: 'np -> 'v) 
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member this.Run(v) = this.Apply(v, (fun n v -> createResultVal n))

type RenderRetC0BaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode)
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this.Apply(v, (fun n v -> v))

type RenderValC1BaseBuilder<'v,'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode, createResultVal: 'np -> 'v) 
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member this.Run(v) = this.Apply(v, (fun n v -> createResultVal n))

type RenderRetC1BaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode)
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this.Apply(v, (fun n v -> v))

type RenderValCnBaseBuilder<'v,'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode, createResultVal: 'np -> 'v) 
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.Apply(v, (fun n v -> createResultVal n))

type RenderRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    >
    (createContext, createThisNode, checkThisNode)
    =
    inherit NodeBuilder<'np,'nc,'d>(createContext, createThisNode, checkThisNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.Apply(v, (fun n v -> v))
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText(op)

type RenderRetC1BaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide v
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText(op)
    
type RenderValC1BaseBuilder<'v,'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText(op)

type RenderRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText(op)

type RenderValCnBaseBuilder<'v,'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText(op)

    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'np,'nc,'d 
        when 'np : equality 
        and 'nc : equality 
        and 'd :> INodeDocument<'np,'nc>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member OnInit(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnEval(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.EvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnAfterEval(this: #NodeBuilder<_,_,_>, m: NodeModifier<_>) =
        do this.AfterEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'n> =
        {
            node: 'n
            evt: 'evt
            gc: GlobalContext
            mutable requestEvaluation: bool
        }
    
    let inline handle node gc (callback: NodeEventArgs<'evt,'n> -> unit) =
        fun evt ->
            let args = { node = node; evt = evt; gc = gc; requestEvaluation = true }
            try
                do gc.evaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    gc.evaluationManager.RequestEvaluation()
            finally
                do gc.evaluationManager.Resume()
