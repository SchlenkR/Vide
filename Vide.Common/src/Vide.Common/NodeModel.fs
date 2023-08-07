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
    abstract member EnsureChildAppended : parent: 'n * child: 'n -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildren : parent: 'n -> 'n list
    abstract member ClearChildren : parent: 'n -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

// We have (abstract NodeContext<'n> + inheritors & INodeDocument<'n>)
// instead of (sealed NodeContext<'n,'d> & 'd :> INodeDocument<'n>)
// because having 'd would make many things very complicated and
// it would require having a node document with an 'e - which would
// prevent having a concrete and completely specialized 'c (see comments below).

[<AbstractClass>] 
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []
    member _.CreateTextNode(value: string) =
        document.CreateTextNode(value)
    member _.ShowChild(child) =
        // What is important here:
        // The ordering is supposed to remain unchanged!
        // So we don't need a concept of "current index"
        do keptChildren <- child :: keptChildren
        do document.EnsureChildAppended(parent, child)
    member _.RemoveObsoleteChildren() =
        let childrenForRemoval = document.GetChildren(parent) |> List.except keptChildren
        for child in childrenForRemoval do
            document.RemoveChild(parent, child)
    member _.ClearContent() =
        do document.ClearChildren(parent)

type BuilderOperations = | Clear

type NodeBuilderState<'e,'s> = option<'e> * option<'s>

type NodeModifierContext<'e> = 
    { 
        node: 'e
        host: IHost
    }

type NodeModifier<'n> = NodeModifierContext<'n> -> unit

// TODO: Since having removed checkChildNode, we could remove 'n.
// Does that open ways for simplification?
[<AbstractClass>]
type NodeBuilder<'e,'c>
    (
        createContext: 'e -> 'c,
        createThisElement: 'c -> 'e
    ) =
    
    inherit VideBaseBuilder()

    member _.Delay(f) = BuilderBricks.delay<_,_,'c>(f)

    member _.CreateContext = createContext
    member _.CreateThisElement = createThisElement

    member val InitModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val PreEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val PostEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get

module NodeBuilder =
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
            NodeBuilder<'e,'n,'c> = NodeBuilder<'e,'n,#NodeContext<'n>>
            
        Also, it is in general required that the builders have a concrete and completely
        specialized 'c (context) in Vide. This ensures smooth composition and overload
        resolution of the builder methods in the CEs, and it makes CE builder methods
        easy to implement (e.g.: see "Combine").

        // TODO: Is this still valid? `checkChildNode` was removed, so maybe we can do this again?
        
        Even if NodeBuilder has now 'e _and also_ 'n, we still have to use an unsafe cast from
        'e to 'n, but having also 'n as arg in NodeBuilder, we can have `checkChildNode`
        taking a 'n instead of an 'e (which otherwise would be wrong).
    *)
    let inline run<'v1,'v2,'s,'e,'n,'c
            when 'n: equality
            and 'c :> NodeContext<'n>
        >
        (thisBuilder: NodeBuilder<'e,'c>)
        (Vide childVide: Vide<'v1,'s,'c>)
        (createResultVal: 'e -> 'v1 -> 'v2)
        : Vide<'v2, NodeBuilderState<'e,'s>, 'c>
        =
        Vide <| fun s gc (parentCtx: 'c) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; host = gc }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisElement,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newElement,s = thisBuilder.CreateThisElement(parentCtx), cs
                    do runModifiers thisBuilder.InitModifiers newElement
                    newElement,s
                | Some thisElement ->
                    do parentCtx.ShowChild(box thisElement :?> 'n)
                    thisElement,cs
            do runModifiers thisBuilder.PreEvalModifiers thisElement
            let thisCtx = thisBuilder.CreateContext(thisElement)
            let cv,cs = childVide cs gc thisCtx
            do thisCtx.RemoveObsoleteChildren()
            do runModifiers thisBuilder.PostEvalModifiers thisElement
            let result = createResultVal thisElement cv
            let state = Some (Some thisElement, cs)
            result,state

module NodeModelBuilderBricks =
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp<'n,'c when 'c :> NodeContext<'n>>(op: BuilderOperations) =
        Vide <| fun s gc (ctx: 'c) ->
            match op with | Clear -> do ctx.ClearContent()
            (),None

    let inline yieldText<'n,'c when 'c :> NodeContext<'n>>(value: string) =
        Vide <| fun s gc (ctx: 'c) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.CreateTextNode(value)
                    do ctx.ShowChild(textNode.node)
                    textNode
                )
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                do ctx.ShowChild(textNode.node)
            (), Some textNode

// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for "vide { .. }" and renderers
// Used for HTML elements like
//    div, p, etc.in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
//   + Combine,For,Delay
// -------
// Note on 
//     "Pot" (has potential return value) and 
//     "Val" (has return value):
//     -> Transformation by "emitValue" member of Pot builders imply that
//          - derived classes have no additional state at all and
//          - "emitValue" is not part of the fluent API and shall be called
//            as the last element in the chain (from a user's perspective).
// ---------------------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n> 
    > () =
    inherit VideBaseBuilder()
    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    member _.Delay(f) = BuilderBricks.delay<_,_,'c>(f)
    member _.Combine(a, b) = BuilderBricks.combine<_,_,_,_,'c>(a, b)
    member _.For(seq, body) = BuilderBricks.for'<_,_,_,'c>(seq, body)

type RenderValC0BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> createResultVal n)

type RenderPotC0BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> v)
    member _.emitValue() = RenderValC0BaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetC0BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> v)

type RenderValC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> createResultVal n)

type RenderPotC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> v)
    member _.emitValue() = RenderValC1BaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> v)

type RenderValCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> createResultVal n)

type RenderPotCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> createResultVal n)
    member _.emitValue() = RenderValCnBaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = NodeBuilder.run this v (fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type VideBaseBuilder with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,'c>) = b {()}
    member _.Yield(b: RenderPotC0BaseBuilder<_,_,_,'c>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,'c>) = b {()}
    member _.Yield(b: RenderValC1BaseBuilder<_,_,_,'c>) = b {()}
    member _.Yield(b: RenderPotC1BaseBuilder<_,_,_,'c>) = b {()}
    member _.Yield(b: RenderRetC1BaseBuilder<_,_,'c>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,'c>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_,'c>) = b {()}
    member _.Yield(v) = NodeModelBuilderBricks.yieldVide(v)
    member _.Yield(op) = NodeModelBuilderBricks.yieldBuilderOp<'n,'c>(op)
    member _.Yield(op) = NodeModelBuilderBricks.yieldText<'n,'c>(op)

    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'n,'c
        when 'c :> NodeContext<'n> 
        and 'n : equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member onInit(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member onEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.PreEvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member onAfterEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.PostEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'e> =
        {
            node: 'e
            evt: 'evt
            host: IHost
            mutable requestEvaluation: bool
        }
    
    let inline handle
        (node: 'e)
        (host: IHost)
        (callback: NodeEventArgs<'evt,'e> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; host = host; requestEvaluation = true }
            try
                do host.SuspendEvaluation()
                do callback args
                if args.requestEvaluation then
                    host.RequestEvaluation()
            finally
                do host.ResumeEvaluation()
