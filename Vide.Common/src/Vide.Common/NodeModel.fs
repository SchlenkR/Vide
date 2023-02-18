[<AutoOpen>]
module Vide.NodeModel

open System.Runtime.CompilerServices
open Vide

type INodeContext<'nchild when 'nchild: equality> =
    inherit IVideContext
    abstract member KeepChild : 'nchild -> unit
    abstract member RemoveObsoleteChildren : unit -> unit
    abstract member ClearContent : unit -> unit
    abstract member AppendChild : 'nchild -> unit

type BuilderOperations = | Clear

type NodeBuilderState<'nthis,'s> = option<'nthis> * option<'s>

type ChildAction = Keep | DiscardAndCreateNew

type NodeModifier<'nthis> = NodeModifierContext<'nthis> -> unit
and NodeModifierContext<'nthis> = { node: 'nthis; services: VideServices }

[<AbstractClass>]
type NodeBuilder<'nthis,'nchild when 'nthis : equality and 'nchild : equality>
    (
        createContext: VideServices -> 'nthis -> INodeContext<'nchild>,
        createThisNode: VideServices -> 'nthis,
        checkChildNode: 'nthis -> ChildAction
    )
    =
    inherit VideBaseBuilder()
    
    member val InitModifiers: ResizeArray<NodeModifier<'nthis>> = ResizeArray() with get
    member val EvalModifiers: ResizeArray<NodeModifier<'nthis>> = ResizeArray() with get
    member val AfterEvalModifiers: ResizeArray<NodeModifier<'nthis>> = ResizeArray() with get

    member this.Apply(
        childVide: Vide<'v,'s,INodeContext<'nchild>>,
        createResultVal: 'nthis -> _ -> _)
        //<'v1,'v2,'s,'n,'nc,'c
        //    when 'nc: equality
        //    and 'c :> INodeContext<'nc>
        //>
        //(Vide childVide: Vide<'v1,'s,'c>)
        //(createResultVal: 'n -> 'v1 -> 'v2)
        //(nodeBuilder: NodeBuilder<'n,'c>)
        //: Vide<'v2, NodeBuilderState<'n,'s>, 'c>
        =
        Vide <| fun s (parentCtx: INodeContext<'nthis>) ->
            Debug.print 0 "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; services = parentCtx.Services }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisNode,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newNode,s = createThisNode parentCtx.Services, cs
                    do parentCtx.AppendChild(newNode)
                    do runModifiers this.InitModifiers newNode
                    newNode,s
                | Some thisNode ->
                    match checkChildNode thisNode with
                    | Keep ->
                        parentCtx.KeepChild(thisNode)
                        thisNode,cs
                    | DiscardAndCreateNew ->
                        createThisNode parentCtx.Services, None
            do runModifiers this.EvalModifiers thisNode
            let thisCtx = createContext parentCtx.Services thisNode
            let cv,cs =
                let (Vide childVide) = childVide
                childVide cs thisCtx
            do thisCtx.RemoveObsoleteChildren()
            do runModifiers this.AfterEvalModifiers thisNode
            let result = createResultVal thisNode cv
            let state = Some (Some thisNode, cs)
            result,state

module BuilderBricks =
    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp<'nc,'c when 'c :> INodeContext<'nc>>(op: BuilderOperations) =
        Vide <| fun s (ctx: 'c) ->
            match op with
            | Clear -> ctx.ClearContent()
            (),None
    
    //let inline yieldText<'nc,'c when 'c :> INodeContext<'nc>>(value: string) =
    //    Vide <| fun s (ctx: 'c) ->
    //        let textNode = s |> Option.defaultWith (fun () -> ctx.AddTextNode(value))
    //        do
    //            if textNode.getText() <> value then
    //                textNode.setText(value)
    //            ctx.KeepChild(textNode.node)
    //        (), Some textNode


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

//module Combine =
//    let combine<'v1,'s1,'n1,'v2,'s2,'n2,'n3,'n4
//            when 'n1 : equality
//            and 'n2 : equality
//            and 'n3 : equality
//        >
//        (
//            Vide a: Vide<'v1,'s1,INodeContext<'n1>>,
//            Vide b: Vide<'v2,'s2,INodeContext<'n2>>
//        )
//        : Vide<'v2,'s1 option * 's2 option,INodeContext<'n3>>
//        =
//        Vide <| fun s ctx ->
//            let sa,sb =
//                match s with
//                | None -> None,None
//                | Some (ms,fs) -> ms,fs
//            let va,sa = a sa ctx
//            let vb,sb = b sb ctx
//            vb, Some (sa,sb)

type ComponentRetCnBaseBuilder<'nchild
        when 'nchild : equality
    > () =
    inherit VideBaseBuilder()
    member _.Return(x) = BuilderBricks.return'(x)
    member _.Delay(f) = BuilderBricks.delay(f)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)

type RenderValC0BaseBuilder<'v,'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'nthis -> 'v) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member this.Run(v) = this.Apply(v, fun n v -> createResultVal n)

type RenderRetC0BaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this.Apply(v, fun n v -> v)

type RenderValC1BaseBuilder<'v,'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'nthis -> 'v) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member this.Run(v) = this.Apply(v, fun n v -> createResultVal n)

type RenderRetC1BaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member _.Return(x) = BuilderBricks.return'(x)
    member this.Run(v) = this.Apply(v, fun n v -> v)

type RenderValCnBaseBuilder<'v,'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode, createResultVal: 'nthis -> 'v) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.Apply(v, fun n v -> createResultVal n)

type RenderRetCnBaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    >
    (createContext, createThisNode, checkChildNode) 
    =
    inherit NodeBuilder<'nthis,'nchild>(createContext, createThisNode, checkChildNode)
    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    member _.For(seq, body) = BuilderBricks.for'(seq, body)
    member this.Run(v) = this.Apply(v, fun n v -> v)
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'nchild
        when 'nchild: equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    // TODO
    //member _.Yield(op) = BuilderBricks.yieldText<'nc,'c>(op)

type RenderRetC1BaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide v
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    // TODO
    //member _.Yield(op) = BuilderBricks.yieldText<'nc,'c>(op)
    
type RenderValC1BaseBuilder<'v,'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    // TODO
    //member _.Yield(op) = BuilderBricks.yieldText<'nc,'c>(op)

type RenderRetCnBaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    // TODO
    //member _.Yield(op) = BuilderBricks.yieldText<'nc,'c>(op)

type RenderValCnBaseBuilder<'v,'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: ComponentRetCnBaseBuilder<_>) = b {()}
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp(op)
    // TODO
    //member _.Yield(op) = BuilderBricks.yieldText<'nc,'c>(op)

    
// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'nthis,'nchild
        when 'nthis: equality
        and 'nchild: equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'nchild
        when 'nchild: equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_>, f) = BuilderBricks.bind(m {()}, f)

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
            services: VideServices
            mutable requestEvaluation: bool
        }
    
    let inline handle (node: 'n) (services: VideServices) (callback: NodeEventArgs<'evt,'n> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; services = services; requestEvaluation = true }
            try
                do services.evaluationManager.Suspend()
                do callback args
                if args.requestEvaluation then
                    services.evaluationManager.RequestEvaluation()
            finally
                do services.evaluationManager.Resume()
