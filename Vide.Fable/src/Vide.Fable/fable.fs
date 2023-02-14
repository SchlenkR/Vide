[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide

type FableDocument() =
    interface INodeDocument<Node> with
        member _.AppendChild(parent, child) =
            parent.appendChild(child) |> ignore
        member _.RemoveChild(parent, child) =
            parent.removeChild(child) |> ignore
        member _.GetChildNodes(parent) =
            let nodes = parent.childNodes
            [ for i in 0 .. nodes.length-1 do nodes.Item i ]
        member _.ClearContent(parent) =
            parent.textContent <- ""
    interface IWebDocument<Node> with
        member _.CreateNodeOfName(tagName) =
            document.createElement tagName
        member _.CreateTextNode(text) =
            let tn = document.createTextNode(text)
            let textNode =
                {
                    node = tn :> Node
                    getText = fun () -> tn.textContent
                    setText = fun value -> tn.textContent <- value
                }
            textNode

type FableContext(parent, evaluationManager) =
    inherit WebContext<Node>(parent, evaluationManager, FableDocument())
    interface INodeContextFactory<Node,FableContext> with
        member _.CreateChildCtx(parent) = FableContext(parent, evaluationManager)

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,FableContext>()

type RenderValC0Builder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v, FableContext,'n,Node>(createNode, checkOrUpdateNode, createResultVal)

type RenderRetC0Builder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderRetC0BaseBuilder<'n,Node,FableContext>(createNode, checkOrUpdateNode)

type RenderValC1Builder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v, FableContext,'n,Node>(createNode, checkOrUpdateNode, createResultVal)

type RenderRetC1Builder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderRetC1BaseBuilder<'n,Node,FableContext>(createNode, checkOrUpdateNode)

type RenderValCnBuilder<'v,'n when 'n :> Node>(createNode, checkOrUpdateNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,FableContext,'n,Node>(createNode, checkOrUpdateNode, createResultVal)

type RenderRetCnBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit RenderRetCnBaseBuilder<'n,Node,FableContext>(createNode, checkOrUpdateNode)


// --------------------------------------------------
// Yields - We have to is here because of 
//          some value restriction issues
// --------------------------------------------------

type ComponentRetCnBuilder with
    member _.Yield(s) = BuilderBricks.yieldText<Node,FableContext> s

type RenderValC1Builder<'v,'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldText s

type RenderRetC1Builder<'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldText s

type RenderValCnBuilder<'v,'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldText s

type RenderRetCnBuilder<'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldText s


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

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

let vide = ComponentRetCnBuilder()
