[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide
open Vide.WebModel

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
        member _.CreateTextNode(text) =
            let tn = document.createTextNode(text)
            do tn.textContent <- text
            let textNode =
                {
                    node = tn :> Node
                    getText = fun () -> tn.textContent
                    setText = fun value -> tn.textContent <- value
                }
            textNode
    interface IWebDocument<Node> with
        member _.CreateNodeOfName(tagName) =
            document.createElement tagName

type FableContext(parent) =
    inherit WebContext<Node>(parent, FableDocument())

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

module FableContext =
    let create<'n when 'n :> Node> (thisNode: 'n) = FableContext(thisNode :> Node)

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,FableContext>()

type RenderValC0Builder<'v,'n when 'n :> Node>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC0Builder<'n when 'n :> Node>(createThisNode, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode)

type RenderValC1Builder<'v,'n when 'n :> Node>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC1Builder<'n when 'n :> Node>(createThisNode, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode)

type RenderValCnBuilder<'v,'n when 'n :> Node>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetCnBuilder<'n when 'n :> Node>(createThisNode, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'n,Node,FableContext>(FableContext.create, createThisNode, checkChildNode)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

module Vide =

    [<GeneralizableValue>]
    let fableContext : Vide<FableContext,unit,FableContext> =
        Vide <| fun s gc ctx -> ctx,None

    [<GeneralizableValue>]
    let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
        Vide <| fun s gc ctx ->
            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
            ctx.Parent :?> 'n,None

module VideApp =
    let inline doCreate appCtor host (content: Vide<'v,'s,FableContext>) onEvaluated =
        let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
        let ctxCtor = fun () -> FableContext(host)
        appCtor content ctxCtor onEvaluated
    let createFable host content onEvaluated =
        doCreate VideApp.create host content onEvaluated
    let createFableWithObjState host content onEvaluated =
        doCreate VideApp.createWithUntypedState host content onEvaluated

let vide = ComponentRetCnBuilder()
