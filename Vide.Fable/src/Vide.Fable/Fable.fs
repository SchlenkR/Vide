[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide
open Vide.WebModel

module Debug =
    let show (n: Node) =
        $"""{n.nodeName}(id={try n.attributes.getNamedItem("id").value.ToString() with _ -> "-"}) """

type FableDocument() =
    interface INodeDocument<Node> with
        member _.EnsureChildAppended(parent, child) =
            if not (parent.contains child) then
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
    interface IWebDocument<Node> with
        member _.CreateNodeOfName(tagName) =
            document.createElement tagName

type FableContext(parent: Node) =
    inherit WebContext<Node>(parent, FableDocument())
    static member Create<'e when 'e :> Node>(thisNode: 'e) = FableContext(thisNode)

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,FableContext>()

type RenderValC0Builder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotC0Builder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderPotC0BaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetC0Builder<'e when 'e :> Node>(createThisElement, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode)

type RenderValC1Builder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotC1Builder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderPotC1BaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetC1Builder<'e when 'e :> Node>(createThisElement, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode)

type RenderValCnBuilder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotCnBuilder<'v,'e when 'e :> Node>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetCnBuilder<'e when 'e :> Node>(createThisElement, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'e,Node,FableContext>(FableContext.Create, createThisElement, checkChildNode)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

module Vide =

    [<GeneralizableValue>]
    let fableContext : Vide<FableContext,unit,FableContext> =
        fun s gc ctx -> ctx,None

    //[<GeneralizableValue>]
    //let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
    //    Vide <| fun s gc ctx ->
    //        // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
    //        ctx.Parent :?> 'n,None

type VideApp =
    static member ForHost(host) = 
        VideAppFactory(
            (fun () -> FableContext(host)),
            (fun (ctx: FableContext) -> ctx.RemoveObsoleteChildren())
        )

let vide = ComponentRetCnBuilder()
