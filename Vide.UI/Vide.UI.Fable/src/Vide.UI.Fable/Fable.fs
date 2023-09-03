namespace Vide

open Browser
open Browser.Types
open Vide
open Vide.WebModel

module Debug =
    let show (n: Node) =
        $"""{n.nodeName}(id={try n.attributes.getNamedItem("id").value.ToString() with _ -> "-"}) """

type FableDocument() =
    interface INodeDocument<Node> with
        member _.EnsureChildAppendedAtIdx(parent, child, idx) =
            let insertChildAtRequestedIdx () = parent.insertBefore(child, parent.childNodes.Item idx) |> ignore
            let childIdx =
                let nodes = parent.childNodes
                let rec loop i =
                    if i >= nodes.length then None
                    elif nodes.Item i = child then Some i
                    else loop (i+1)
                loop 0
            // insert child at idx if it's not already there
            match childIdx with
            | None -> insertChildAtRequestedIdx ()
            | Some childIdx when childIdx = idx -> ()
            | _ ->
                parent.removeChild(child) |> ignore
                insertChildAtRequestedIdx ()
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

module FableContext =
    let create<'e when 'e :> Node> (host: IHost) (thisNode: 'e) = FableContext(thisNode)

// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Node,FableContext>()

type RenderValC0Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderPotC0Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderPotC0BaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderRetC0Builder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetC0BaseBuilder<'e,Node,FableContext>(FableContext.create, createThisElement)

type RenderValC1Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderPotC1Builder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderPotC1BaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderRetC1Builder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetC1BaseBuilder<'e,Node,FableContext>(FableContext.create, createThisElement)

type RenderValCnBuilder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderPotCnBuilder<'v,'e when 'e :> Node>(createThisElement, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Node,FableContext>(FableContext.create, createThisElement, createResultVal)
type RenderRetCnBuilder<'e when 'e :> Node>(createThisElement) =
    inherit RenderRetCnBaseBuilder<'e,Node,FableContext>(FableContext.create, createThisElement)


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

// TODO:
// module Vide =
//     [<GeneralizableValue>]
//     let fableContext : Vide<FableContext,unit,HostContext<FableContext>> =
//         Vide <| fun s ctx -> ctx,None

//     [<GeneralizableValue>]
//     let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
//        Vide <| fun s host ctx ->
//            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
//            ctx.Parent :?> 'n,None

type VideApp =
    static member ForHost(host) = 
        VideAppFactory(
            (fun () -> FableContext(host)),
            (fun ctx -> do ctx.ctx.RemoveObsoleteChildren())
        )

[<AutoOpen>]
module VideBuilderInstance =
    let vide = ComponentRetCnBuilder()
