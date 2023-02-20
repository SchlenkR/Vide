[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide

type FableDocument<'np when 'np :> Node> () =
    interface INodeDocument<'np,Node> with
        member _.AppendChild(parent, child) =
            parent.appendChild(child) |> ignore
        member _.RemoveChild(parent, child) =
            parent.removeChild(child) |> ignore
        member _.GetChildren(parent) =
            let nodes = parent.childNodes
            [ for i in 0 .. nodes.length-1 do nodes.Item i ]
        member _.ClearChildren(parent) =
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

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

module FableContext =
    let create<'np when 'np :> Node and 'np : equality> (thisNode: 'np) = 
        NodeContext(thisNode, FableDocument<'np>())

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,Node,FableDocument<Node>>()

type RenderValC0Builder<'v,'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC0Builder<'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode)

type RenderValC1Builder<'v,'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetC1Builder<'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode)

type RenderValCnBuilder<'v,'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode, createResultVal)

type RenderRetCnBuilder<'n when 'n :> Node and 'n : equality>(createThisNode, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'n,Node,FableDocument<'n>>(FableContext.create, createThisNode, checkChildNode)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

//module Vide =

//    [<GeneralizableValue>]
//    let fableContext : Vide<FableContext,unit,FableContext> =
//        Vide <| fun s gc ctx -> ctx,None

    //[<GeneralizableValue>]
    //let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
    //    Vide <| fun s gc ctx ->
    //        // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
    //        ctx.Parent :?> 'n,None

module BuilderHelper =
    let createNode<'e> tagName =
        let n = document.createElement tagName
        (box n) :?> 'e
    
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew

module VideApp =
    type FableContext<'np when 'np :> Node and 'np : equality> =
        NodeContext<'np,Node,FableDocument<'np>>
    
    let inline doCreate appCtor host (content: Vide<_,_,_>) onEvaluated =
        let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
        let ctxCtor = fun () -> FableContext.create(host)
        appCtor content ctxCtor onEvaluated
    let createFable host content onEvaluated =
        doCreate VideApp.create host content onEvaluated
    let createFableWithObjState host content onEvaluated =
        doCreate VideApp.createWithUntypedState host content onEvaluated

let vide = ComponentRetCnBuilder()
