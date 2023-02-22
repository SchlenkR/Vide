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

/// This must be a stateless implementation that abstracts
/// commonly used node functions. Note that from the
/// point of view of this interface, parent and child are
/// both 'of type 'n (which is the common node type).
type INodeDocument<'n> =
    abstract member AppendChild : parent: 'n * child: 'n -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildren : parent: 'n -> 'n list
    abstract member ClearChildren : parent: 'n -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

[<AbstractClass>] 
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []
    member _.NodeDocument = document
    member _.KeepChildNode(child) =
        do keptChildren <- child :: keptChildren
    member this.AppendChild(child) =
        do this.KeepChildNode(child)
        do document.AppendChild(parent, child)
    member _.RemoveObsoleteChildren() =
        do 
            document.GetChildren(parent)
            |> List.except keptChildren
            |> List.iter (fun child -> document.RemoveChild(parent, child))
    member _.ClearContent() =
        do document.ClearChildren(parent)

type BuilderOperations = | Clear

type NodeBuilderState<'e,'s> = option<'e> * option<'s>

type ChildAction = Keep | DiscardAndCreateNew

type NodeModifierContext<'e> =
    {
        node: 'e
        globalContext: GlobalContext
    }

type NodeModifier<'n> = NodeModifierContext<'n> -> unit

[<AbstractClass>]
type NodeBuilder<'e,'c>
    (
        createContext: 'e -> 'c,
        createThisNode: 'c -> 'e,
        checkChildNode: 'e -> ChildAction
    ) =
    
    inherit VideBaseBuilder()

    member _.CreateContext = createContext
    member _.CreateThisNode = createThisNode
    member _.CheckChildNode = checkChildNode

    member val InitModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get

module ModifierContext =
    // TODO: This is really, really weired. I think it's necessary to distinguish
    // between 'nthis and 'nhild on a general level (see branch of 2023-02-18 and many others)
    (*
        Generic Argument names:
        ---

        'e : A concrete element type, e.g. `HTMLButtonElement`
        'n : The abstract node type that is the basis for
             tree composition (e.g. `Node`)
        ...

        Notes
        ---
        
        In order to get rid of the unsafe cast, we need a constraint in form of ('e :> 'n),
        which is not possible. The interesting thing to see here is the type of "this":
            NodeBuilder<'e,'c> = NodeBuilder<'e, #NodeContext<'n>>
            
        Also, it is in general required that the builders have a concrete and completely
        specialized 'c (context) in Vide. This ensures smooth composition and overload
        resolution of the builder methods in the CEs, and it makes CE builder methods
        easy to implement (e.g.: see "Combine").
    *)
    let inline apply<'v1,'v2,'s,'e,'n,'c
            when 'n: equality
            and 'c :> NodeContext<'n>
        >
        (Vide childVide: Vide<'v1,'s,'c>)
        (createResultVal: 'e -> 'v1 -> 'v2)
        (this: NodeBuilder<'e,'c>)
        : Vide<'v2, NodeBuilderState<'e,'s>, 'c>
        =
        Vide <| fun s gc (parentCtx: 'c) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; globalContext = gc }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisNode,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newNode,s = this.CreateThisNode(parentCtx), cs
                    do runModifiers this.InitModifiers newNode
                    newNode,s
                | Some node ->
                    match this.CheckChildNode(node) with
                    | Keep ->
                        parentCtx.KeepChildNode((box node) :?> 'n)
                        node,cs
                    | DiscardAndCreateNew ->
                        this.CreateThisNode(parentCtx), None
            do runModifiers this.EvalModifiers thisNode
            let childCtx =
                // TODO: Why the unsafe cast everywhere in this function?
                this.CreateContext thisNode
            let cv,cs = childVide cs gc childCtx
            do childCtx.RemoveObsoleteChildren()
            do runModifiers this.AfterEvalModifiers thisNode
            let result = createResultVal thisNode cv
            let state = Some (Some thisNode, cs)
            result,state

module BuilderBricks =
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp<'n,'c when 'c :> NodeContext<'n>>(op: BuilderOperations) =
        Vide <| fun s gc (ctx: 'c) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None

    let inline yieldText<'n,'c when 'c :> NodeContext<'n>>(value: string) =
        Vide <| fun s gc (ctx: 'c) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.NodeDocument.CreateTextNode(value)
                    do ctx.AppendChild(textNode.node)
                    textNode
                )
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                ctx.KeepChildNode(textNode.node)
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

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n> 
    > () =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    member _.Delay(f) = BuilderBricks.delay(f)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)

type RenderValC0BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC0BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> v)

type RenderValC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> v)

type RenderValCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> createResultVal n)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisNode, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this |> ModifierContext.apply v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n> 
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide v
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)
    
type RenderValC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)

type RenderValCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)

    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'n,'c
        when 'c :> NodeContext<'n> 
        and 'n : equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

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
    type NodeEventArgs<'evt,'e> =
        {
            node: 'e
            evt: 'evt
            gc: GlobalContext
            mutable requestEvaluation: bool
        }
    
    let inline handle
        (node: 'e)
        (gc: GlobalContext)
        (callback: NodeEventArgs<'evt,'e> -> unit)
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
