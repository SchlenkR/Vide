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

// TODO: Comment (in general) why 'np,'nc attempt is not preferred over just 'n

/// This must be stateless!
type INodeDocument<'n> =
    abstract member AppendChild : child: 'n -> unit
    abstract member RemoveChild : child: 'n -> unit
    abstract member GetChildren : unit -> 'n list
    abstract member ClearChildren : unit -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

[<Sealed>] 
type NodeContext<'n,'d 
        when 'n: equality 
        and 'd :> INodeDocument<'n>
    >
    (document: 'd) 
    =
    let mutable keptChildren = []
    member _.NodeDocument = document
    member _.KeepChild(child) =
        do keptChildren <- child :: keptChildren
    member this.AppendChild(child) =
        do this.KeepChild(child)
        do document.AppendChild(child)
    member _.RemoveObsoleteChildren() =
        do 
            document.GetChildren()
            |> List.except keptChildren
            |> List.iter (fun child -> document.RemoveChild(child))
    member _.ClearContent() =
        do document.ClearChildren()

type BuilderOperations = | Clear

type NodeBuilderState<'n,'s> = option<'n> * option<'s>

type ChildAction = Keep | DiscardAndCreateNew

type NodeModifierContext<'d> =
    {
        document: 'd
        globalContext: GlobalContext
    }

type NodeModifier<'d> = NodeModifierContext<'d> -> unit

// we need this to prevent value restrictions
// (TODO: why exactly - why not before?)
[<AbstractClass>]
type VideBaseBuilder<'n,'d 
        when 'n : equality 
        and 'd :> INodeDocument<'n>
    > () =
    member _.Bind(m, f) = BuilderBricks.bind<_,_,_,_,_> (m, f)
    member _.Zero() = BuilderBricks.zero<_> ()
    member _.Delay(f) = BuilderBricks.delay<_,_,_> (f)

    // ---------------------
    // ASYNC
    // ---------------------
    //member _.Bind(m, f) = BuilderBricks.Async.bind<_,_,_,_> (m, f)
    //member _.Delay(f) = BuilderBricks.Async.delay<_,_> (f)
    //member _.Combine(a, b) = BuilderBricks.Async.combine<_,_,_,_,_> (a, b)
    // TODO: async for

[<AbstractClass>]
type NodeBuilder<'n,'d 
        when 'n : equality 
        and 'd :> INodeDocument<'n>
    >
    (
        createNodeAndDocument: unit -> 'n * 'd,
        checkChildNode: 'n -> ChildAction
    )
    =
    inherit VideBaseBuilder<'n,'d>()

    member _.CreateNodeAndDocument = createNodeAndDocument
    member _.CheckChildNode = checkChildNode

    member val InitModifiers: ResizeArray<NodeModifier<'d>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'d>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'d>> = ResizeArray() with get

// TODO: Member von NodeBuilder
module ModifierContext =
    // TODO: This is really, really weired. I think it's necessary to distinguish
    // between 'nthis and 'nchild on a general level (see branch of 2023-02-18)
    let inline apply
        (Vide childVide: Vide<'v1,_,_>)
        (createResultVal: _ -> 'v1 -> 'v2)
        (this: NodeBuilder<_,_>)
        =
        Vide <| fun s gc (parentCtx: NodeContext<'n,'d>) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers document =
                for modifier in modifiers do
                    modifier { document = document; globalContext = gc }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisNode,document,cs =
                let createAndAppendThisNodeAnndDocument () =
                    let newNode,document = this.CreateNodeAndDocument()
                    do parentCtx.AppendChild(newNode)
                    newNode,document
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let (newNode,document) = createAndAppendThisNodeAnndDocument ()
                    do runModifiers this.InitModifiers document
                    newNode,document,cs
                | Some (node,document) ->
                    match this.CheckChildNode(node) with
                    | Keep ->
                        do parentCtx.KeepChild(node)
                        node,document,cs
                    | DiscardAndCreateNew ->
                        let node,document = createAndAppendThisNodeAnndDocument ()
                        node,document,None
            do runModifiers this.EvalModifiers document
            let childCtx= NodeContext(document)
            let cv,cs = childVide cs gc childCtx
            do childCtx.RemoveObsoleteChildren()
            do runModifiers this.AfterEvalModifiers document
            let result = createResultVal document cv
            let state = Some (Some (thisNode,document), cs)
            result,state

module BuilderBricks =
    let inline yieldVide (v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp (op: BuilderOperations) =
        Vide <| fun s gc (ctx: NodeContext<_,_>) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None

    let inline yieldText<'n,'d when 'n: equality and 'd :> INodeDocument<'n>> (value: string) =
        Vide <| fun s gc (ctx: NodeContext<'n,'d>) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.NodeDocument.CreateTextNode(value)
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

type ComponentRetCnBaseBuilder<'n,'d 
        when 'n: equality
        and 'd :> INodeDocument<'n>
    > () =
    inherit VideBaseBuilder<'n,'d>()
    member _.Return(x) = BuilderBricks.return'<_,_> (x)
    member _.Delay(f) = BuilderBricks.delay<_,_,_> (f)
    member _.Combine(a, b) = BuilderBricks.combine<_,_,_,_,_> (a, b)
    member _.For(seq, body) = BuilderBricks.for'<_,_,_,_> (seq, body)

type RenderValC0BaseBuilder<'v,'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode, createResultVal: 'd -> 'v)
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> createResultVal d)

type RenderRetC0BaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode) 
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member _.Return(x) = BuilderBricks.return'<_,_> (x)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> v)

type RenderValC1BaseBuilder<'v,'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode, createResultVal: 'd -> 'v) 
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> createResultVal d)

type RenderRetC1BaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode) 
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member _.Return(x) = BuilderBricks.return'<_,_> (x)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> v)

type RenderValCnBaseBuilder<'v,'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode, createResultVal: 'd -> 'v) 
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine<_,_,_,_,_> (a, b)
    member _.For(seq, body) = BuilderBricks.for'<_,_,_,_> (seq, body)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> createResultVal d)

type RenderRetCnBaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>>
    (createNodeAndDocument, checkChildNode)
    =
    inherit NodeBuilder<'n,'d>(createNodeAndDocument, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine<_,_,_,_,_> (a, b)
    member _.For(seq, body) = BuilderBricks.for'<_,_,_,_> (seq, body)
    member _.Return(x) = BuilderBricks.return'<_,_> (x)
    member this.Run(v) = this |> ModifierContext.apply v (fun d v -> v)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<'n,'d>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'d>(op)

type RenderRetC0BaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<'n,'d>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'d>(op)

type RenderRetC1BaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<'n,'d>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide v
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'d>(op)

type RenderValCnBaseBuilder<'v,'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<'n,'d>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'d>(op)

type RenderRetCnBaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,'n,'d>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<'n,'d>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'d>(op)

    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<'n,'d>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetC1BaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<'n,'d>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'e,'n,'d when 'n: equality and 'd :> INodeDocument<'n>> with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,'n,'d>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<'n,'d>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member OnInit(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.EvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member OnAfterEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
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
    
    let inline handle
        (node: 'n) 
        (gc: GlobalContext) 
        (callback: NodeEventArgs<'evt,'n> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; gc = gc; requestEvaluation = true }
            try
                do gc.evaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    gc.evaluationManager.RequestEvaluation()
            finally
                do gc.evaluationManager.Resume()
