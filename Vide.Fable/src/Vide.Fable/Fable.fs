[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide
open Vide.WebModel

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
        Vide <| fun s gc ctx -> ctx,None

    //[<GeneralizableValue>]
    //let node<'n when 'n :> Node> : Vide<'n,unit,FableContext> =
    //    Vide <| fun s gc ctx ->
    //        // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
    //        ctx.Parent :?> 'n,None

module VideApp =
    module Fable =
        let inline doCreate appCtor host (content: Vide<'v,'s,FableContext>) =
            let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
            let ctxCtor = fun () -> FableContext(host)
            appCtor content ctxCtor
        let create host content =
            doCreate VideApp.create host content
        let createWithUntypedState host content =
            doCreate VideApp.createWithUntypedState host content
        let createAndStart host content =
            doCreate VideApp.createAndStart host content
        let createAndStartWithUntypedState host content =
            doCreate VideApp.createAndStartWithUntypedState host content

let vide = ComponentRetCnBuilder()

[<AutoOpen>]
module Keywords =
    
    let caseWithBehavior 
            (guardValue: 'g)
            caseType
            (view: Vide<_,_,_>) 
            (elseBehavior: Vide<_,_,_>)
            (switchState: ('g -> bool) * bool * Vide<_,_,_>)
        =
        let guard,caseMatchedBefore,currView = switchState
        let cond =
            let innerCond = guard guardValue
            match caseType with
            | CaseType.First -> innerCond
            | CaseType.Always -> (not caseMatchedBefore) && innerCond
        let resultingView =
            vide {
                currView
                if cond then view else elseBehavior
            }
        guard, (caseMatchedBefore || cond), resultingView
        
    let case cond view switchState =
        caseWithBehavior cond CaseType.First view elsePreserve switchState
    let caseAnd cond view switchState =
        caseWithBehavior cond CaseType.Always view elsePreserve switchState
    let caseForget cond view switchState =
        caseWithBehavior cond CaseType.First view elseForget switchState
    let caseForgetAnd cond view switchState =
        caseWithBehavior cond CaseType.Always view elseForget switchState
        
    let caseDefaultWithBehaviour
            (view: Vide<_,_,_>) 
            (elseBehavior: Vide<_,_,_>)
            (switchState: ('g -> bool) * bool * Vide<_,_,_>)
        =
        let _,caseMatchedBefore,currView = switchState
        vide {
            currView
            if not caseMatchedBefore then view else elseBehavior
        }
    let caseDefault (view: Vide<_,_,_>) switchState =
        caseDefaultWithBehaviour view elsePreserve switchState
    let caseDefaultForget (view: Vide<_,_,_>) switchState =
        caseDefaultWithBehaviour view elseForget switchState
