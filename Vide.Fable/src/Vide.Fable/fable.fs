[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide
open Vide.WebModel

// Abstract FableDocumentApi and other stuff again in "Web..."

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

type FableDocument() =
    member _.AppendChild(parent: #Node, child) =
        parent.appendChild(child) |> ignore
    member _.RemoveChild(parent: #Node, child) =
        parent.removeChild(child) |> ignore
    member _.GetChildNodes(parent: #Node) =
        let nodes = parent.childNodes
        [ for i in 0 .. nodes.length-1 do nodes.Item i ]
    member _.ClearContent(parent: #Node) =
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

type FableContext<'nthis
        when 'nthis :> Node 
        and 'nthis : equality
    >
    (thisNode: 'nthis, videServices: VideServices)
    =
    let mutable keptChildren = []
    
    let document = FableDocument()
    let keepChild (child: Node) = keptChildren <- child :: keptChildren

    interface IVideContext with
        member _.Services = videServices
    interface INodeContext<Node> with
        member _.KeepChild(child) =
            do keepChild child
        member _.RemoveObsoleteChildren() =
            do
                document.GetChildNodes(thisNode)
                |> List.except keptChildren
                |> List.iter (fun child -> document.RemoveChild(thisNode, child))
        member _.ClearContent() =
            do document.ClearContent(thisNode)
        member _.AppendChild(child) =
            do keepChild (child)
            do document.AppendChild(thisNode, child)

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

module FableContextFactory =
    let create<'nthis,'nchild
            when 'nthis :> Node 
            and 'nchild :> Node
            and 'nthis : equality
            and 'nchild : equality
        >
        (videServices: VideServices) thisNode
        =
        FableContext<'nthis>(thisNode, videServices) :> INodeContext<Node>

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node>()

type RenderValC0Builder<'v,'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC0Builder<'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode)

type RenderValC1Builder<'v,'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC1Builder<'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode)

type RenderValCnBuilder<'v,'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode, createResultVal)

type RenderRetCnBuilder<'nthis when 'nthis :> Node and 'nthis : equality>(createThisNode, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'nthis,Node>(
        FableContextFactory.create, createThisNode, checkChildNode)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

// TODO
//module Vide =
//    [<GeneralizableValue>]
//    let fableContext : Vide<FableContext,unit,FableContext> =
//        Vide <| fun s ctx -> ctx,None
//    [<GeneralizableValue>]
//    let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
//        Vide <| fun s ctx ->
//            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
//            ctx.Parent :?> 'n,None

//module VideApp =
//    let inline doCreate appCtor (host: #Node) (content: Vide<'v,'s,FableContext>) onEvaluated =
//        let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
//        let ctxCtor = fun eval -> FableContext(host, eval)
//        appCtor content ctxCtor onEvaluated
//    let createFable host content onEvaluated =
//        doCreate VideApp.create host content onEvaluated
//    let createFableWithObjState host content onEvaluated =
//        doCreate VideApp.createWithUntypedState host content onEvaluated



module BuilderHelper =
    let createNode<'n when 'n :> HTMLElement> tagName (_: VideServices) =
        document.createElement(tagName) :?> 'n

let vide = ComponentRetCnBuilder()
