[<AutoOpen>]
module Vide.UI.WebSharper

// ==================================================================
// Fable <-> WebSharper
// --------------------
// - The difference between FableDocument/FableContext and the WebSharper
//   counterparts is just pascal case naming.
// - namespaces
// ==================================================================

open WebSharper.JavaScript
open WebSharper.JavaScript.Dom

open Vide
open Vide.WebModel

type WebSharperDocument() =
    interface INodeDocument<Node> with
        member _.AppendChild(parent, child) =
            parent.AppendChild(child) |> ignore
        member _.RemoveChild(parent, child) =
            parent.RemoveChild(child) |> ignore
        member _.GetChildren(parent) =
            let nodes = parent.ChildNodes
            [ for i in 0 .. nodes.Length - 1 do nodes.Item i ]
        member _.ClearChildren(parent) =
            parent.TextContent <- ""
        member _.CreateTextNode(text) =
            let tn = JS.Document.CreateTextNode(text)
            do tn.TextContent <- text
            let textNode =
                {
                    node = tn :> Node
                    getText = fun () -> tn.TextContent
                    setText = fun value -> tn.TextContent <- value
                }
            textNode
    interface IWebDocument<Node> with
        member _.CreateNodeOfName(tagName) =
            JS.Document.CreateElement(tagName)

type WebSharperContext(parent) =
    inherit WebContext<Node>(parent, WebSharperDocument())
    static member Create<'e when 'e :> Node>(thisNode: 'e) = WebSharperContext(thisNode :> Node)

// ==================================================================
// Fable <-> WebSharper
// --------------------
// From here, everything is completely the same except naming of document
// and context. Are those things really needed (i.e. specialization) or
// can we leave them general?
// ==================================================================


// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,WebSharperContext>()

type RenderValC0Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement, createResultVal)

type RenderRetC0Builder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetC0BaseBuilder<'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement)

type RenderValC1Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement, createResultVal)

type RenderRetC1Builder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetC1BaseBuilder<'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement)

type RenderValCnBuilder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement, createResultVal)

type RenderRetCnBuilder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetCnBaseBuilder<'e,Node,WebSharperContext>(WebSharperContext.Create, createThisElement)


// --------------------------------------------------
// Yields - We have to is here because of 
//          some value restriction issues
// --------------------------------------------------

module BuilderBricks =
    let yieldWebSharperText s = BuilderBricks.yieldText<Node, WebSharperContext> s

type ComponentRetCnBuilder with
    member _.Yield(s) = BuilderBricks.yieldWebSharperText s

type RenderValC1Builder<'v,'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldWebSharperText s

type RenderRetC1Builder<'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldWebSharperText s

type RenderValCnBuilder<'v,'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldWebSharperText s

type RenderRetCnBuilder<'n when 'n :> Node> with
    member _.Yield(s) = BuilderBricks.yieldWebSharperText s


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

// TODO

//module Vide =

//    [<GeneralizableValue>]
//    let webSharperContext : Vide<WebSharperContext,unit,WebSharperContext> =
//        Vide <| fun s ctx -> ctx,None

//    [<GeneralizableValue>]
//    let node<'n when 'n :> Node> : Vide<'n,unit,WebSharperContext> =
//        Vide <| fun s ctx ->
//            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
//            ctx.Parent :?> 'n,None

// TODO: Doesn't compile in WebSharper
module VideApp =
    module WebSharper =
        let inline doCreate appCtor (host: #Node) (content: Vide<'v,'s,WebSharperContext>) onEvaluated =
            let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
            let ctxCtor = fun () -> WebSharperContext(host)
            appCtor content ctxCtor onEvaluated
        let create host content onEvaluated =
            doCreate VideApp.create host content onEvaluated
        let createWithUntypedState host content onEvaluated =
            doCreate VideApp.createWithUntypedState host content onEvaluated

let vide = ComponentRetCnBuilder()
