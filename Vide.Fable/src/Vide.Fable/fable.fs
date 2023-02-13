[<AutoOpen>]
module Vide.Fable

open Vide
open Browser
open Browser.Types

type FableDocument() =
    interface INodeDocument<Node> with
        member _.CreateElementByName(tagName) =
            document.createElement tagName
        member _.CreateText(text) =
            let tn = document.createTextNode(text)
            let textNode =
                {
                    node = tn :> Node
                    getText = fun () -> tn.textContent
                    setText = fun value -> tn.textContent <- value
                }
            textNode
        member _.AppendChild(parent, child) =
            parent.appendChild(child) |> ignore
        member _.RemoveChild(parent, child) =
            parent.removeChild(child) |> ignore
        member _.GetChildNodes(parent) =
            let nodes = parent.childNodes
            [ for i in 0 .. nodes.length-1 do nodes.Item i ]
        member _.ClearContent(parent) =
            parent.textContent <- ""

type FableContext(parent, evaluationManager) =
    inherit NodeContext<Node>(parent, evaluationManager, FableDocument())
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
    member _.Yield(b: ComponentRetCnBuilder<_>) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText<Node,FableContext> s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

type ComponentRetCnBuilder<'c> with
    member _.Yield(b: RenderValC0Builder<_,_>) = b {()}
    member _.Yield(b: RenderRetC0Builder<_>) = b {()}
    member _.Yield(b: RenderRetCnBuilder<_>) = b {()}
    member _.Yield(b: ComponentRetCnBuilder<_>) = b {()}
    member _.Yield(s) = BuilderBricks.yieldText<Node,FableContext> s
    member _.Yield(v) = BuilderBricks.yieldVide(v)
    member _.Yield(op) = BuilderBricks.yieldBuilderOp op

// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetCnBuilder<'n when 'n :> Node> with
    member _.Bind(m: RenderValC0Builder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0Builder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBuilder<'c> with
    member _.Bind(m: RenderValC0Builder<_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0Builder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBuilder<_>, f) = BuilderBricks.bind(m {()}, f)

module Vide =

    [<GeneralizableValue>]
    let fableContext : Vide<FableContext,unit,FableContext> =
        Vide <| fun s ctx -> ctx,None

    [<GeneralizableValue>]
    let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
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

let vide = ComponentRetCnBuilder<FableContext>()
