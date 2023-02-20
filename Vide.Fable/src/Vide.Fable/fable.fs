namespace Vide.Fable

open Browser
open Browser.Types
open Vide

type FableDocument<'e when 'e :> HTMLElement>(thisNode: 'e) =
    member _.Node = thisNode
    interface INodeDocument<Node> with
        member _.AppendChild(child) =
            thisNode.appendChild(child) |> ignore
        member _.RemoveChild(child) =
            thisNode.removeChild(child) |> ignore
        member _.GetChildren() =
            let nodes = thisNode.childNodes
            [ for i in 0 .. nodes.length-1 do nodes.Item i ]
        member _.ClearChildren() =
            thisNode.textContent <- ""
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
    member _.CreateNodeOfName(tagName) =
        document.createElement tagName

type FableContext<'e when 'e :> HTMLElement> = NodeContext<Node,FableDocument<'e>>

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

module Helper =
    let createNodeAndDocument<'e when 'e :> HTMLElement> (createThisNode: unit -> 'e) =
        fun () ->
            let e = createThisNode ()
            let ctx = FableDocument(e)
            e :> Node,ctx

type ComponentRetCnBuilder<'e when 'e :> HTMLElement>() =
    inherit ComponentRetCnBaseBuilder<Node,FableDocument<'e>>()

type RenderValC0Builder<'v,'e when 'e :> HTMLElement>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode, createResultVal)

type RenderRetC0Builder<'e when 'e :> HTMLElement>(createThisNode, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode)

type RenderValC1Builder<'v,'e when 'e :> HTMLElement>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode, createResultVal)

type RenderRetC1Builder<'e when 'e :> HTMLElement>(createThisNode, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode)

type RenderValCnBuilder<'v,'e when 'e :> HTMLElement>(createThisNode, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode, createResultVal)

type RenderRetCnBuilder<'e when 'e :> HTMLElement>(createThisNode, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'e,Node,FableDocument<'e>>(
        Helper.createNodeAndDocument<'e> createThisNode, checkChildNode)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

//module Vide =
//    [<GeneralizableValue>]
//    let fableContext : Vide<FableContext,unit,FableContext> =
//        Vide <| fun s gc ctx -> ctx,None
//    [<GeneralizableValue>]
//    let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
//        Vide <| fun s gc ctx ->
//            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
//            ctx.Parent :?> 'n,None

module FableApp =
    let inline doCreate appCtor host (content: Vide<_,_,_>) onEvaluated =
        // the "yield" is needed - to infer the correct type of "content"
        // (or as an alternative: specify it in the signature and omit "yield")
        let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { yield content }
        let ctxCtor = fun () -> FableContext(host)
        appCtor content ctxCtor onEvaluated
    let create host content onEvaluated =
        doCreate VideApp.create host content onEvaluated
    let createWithObjState host content onEvaluated =
        doCreate VideApp.createWithUntypedState host content onEvaluated


// we have to shadow some functions to prevent value restriction issues
// TODO: Again - why does that occur after the refactoring?
module Fable =
    let ofMutable value = Vide.ofMutable<FableContext<HTMLElement>, _> value

[<AutoOpen>]
module Defaults =
    
    [<GeneralizableValue>]
    let vide<'e when 'e :> HTMLElement> = ComponentRetCnBuilder<'e>()
