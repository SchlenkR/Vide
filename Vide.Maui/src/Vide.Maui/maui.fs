namespace Vide.Maui

open System
open System.Collections.Generic
open Microsoft.Maui
open Microsoft.Maui.Controls
open Vide

// The current basis for vide-implementations is the
// NodeModel (with NodeDocument and NodeContext), which
// actually works like a true node model (always many children).
// The node model doesn't distinguish between different container
// forms (e.g. single / multi content), so there are some casts
// needed here and runtime exceptions can occur. Anyway, content models can be modeled correctly at the API
// surface, using the Render-builders.
type MauiDocument() =
    let makeEx parent child actionName =
        let childTypeName = match child with Some child -> child.GetType().Name | None -> "-"
        Exception $"Cannot perform '{actionName}' to node of type {parent.GetType().Name} (child type = {childTypeName})."
    interface INodeDocument<IView> with
        member _.AppendChild(parent, child) =
            match box parent with
            | :? ICollection<IView> as viewColl -> viewColl.Add(child)
            | :? ContentView as contentView -> contentView.Content <- child :?> View
            | _ -> raise <| makeEx parent (Some child) "append child"
        member _.RemoveChild(parent, child) =
            match box parent with
            | :? ICollection<IView> as viewColl -> viewColl.Remove(child) |> ignore
            | :? ContentView as contentView -> contentView.Content <- null
            | _ -> raise <| makeEx parent (Some child) "remove child"
        member _.GetChildNodes(parent) =
            match box parent with
            | :? ICollection<IView> as viewColl -> viewColl |> Seq.toList
            | :? ContentView as contentView -> contentView.Content :> IView |> List.singleton
            | _ -> []
        member _.ClearContent(parent) =
            match box parent with
            | :? ICollection<IView> as viewColl -> viewColl.Clear()
            | :? ContentView as contentView -> contentView.Content <- null
            | _ -> raise <| makeEx parent None "clear content"
        member _.CreateTextNode(text) =
            let tn = Label()
            do tn.Text <- text
            let textNode =
                {
                    node = tn :> IView
                    getText = fun () -> tn.Text
                    setText = fun value -> tn.Text <- value
                }
            textNode

type MauiContext
    (
        parent,
        evaluationManager,
        document: MauiDocument
    ) =
    inherit NodeContext<IView>(parent, evaluationManager, document)
    interface INodeContextFactory<IView,MauiContext> with
        member _.CreateChildCtx(parent) = MauiContext(parent, evaluationManager, document)
    member this.AddElement() =
        let n = new 'n()
        do this.KeepChild n
        do (document :> INodeDocument<_>).AppendChild(parent, n)
        n


// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<IView,MauiContext>()

type RenderValC0Builder<'v,'n when 'n :> IView>(createNode, checkNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'n,IView,MauiContext>(createNode, checkNode, createResultVal)

type RenderRetC0Builder<'n when 'n :> IView>(createNode, checkNode) =
    inherit RenderRetC0BaseBuilder<'n,IView,MauiContext>(createNode, checkNode)

type RenderValC1Builder<'v,'n when 'n :> IView>(createNode, checkNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'n,IView,MauiContext>(createNode, checkNode, createResultVal)

type RenderRetC1Builder<'n when 'n :> IView>(createNode, checkNode) =
    inherit RenderRetC1BaseBuilder<'n,IView,MauiContext>(createNode, checkNode)

type RenderValCnBuilder<'v,'n when 'n :> IView>(createNode, checkNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'n,IView,MauiContext>(createNode, checkNode, createResultVal)

type RenderRetCnBuilder<'n when 'n :> IView>(createNode, checkNode) =
    inherit RenderRetCnBaseBuilder<'n,IView,MauiContext>(createNode, checkNode)


// --------------------------------------------------
// Yields - We have to is here because of 
//          some value restriction issues
// --------------------------------------------------

module BuilderBricks =
    let yieldMauiText s = BuilderBricks.yieldText<IView,MauiContext> s

type ComponentRetCnBuilder with
    member _.Yield(s) = BuilderBricks.yieldMauiText s

type RenderValC1Builder<'v,'n when 'n :> IView> with
    member _.Yield(s) = BuilderBricks.yieldMauiText s

type RenderRetC1Builder<'n when 'n :> IView> with
    member _.Yield(s) = BuilderBricks.yieldMauiText s

type RenderValCnBuilder<'v,'n when 'n :> IView> with
    member _.Yield(s) = BuilderBricks.yieldMauiText s

type RenderRetCnBuilder<'n when 'n :> IView> with
    member _.Yield(s) = BuilderBricks.yieldMauiText s


// --------------------------------------------------
// Specialized vide functions
// --------------------------------------------------

//module Vide =

//    // all this because of value restrictions / quite redundant with Fable?

//    [<GeneralizableValue>]
//    let mauiContext : Vide<MauiContext,unit,MauiContext> =
//        Vide <| fun s ctx -> ctx,None

//    [<GeneralizableValue>]
//    let node<'n when 'n :> IView> : Vide<'n,unit,MauiContext> =
//        Vide <| fun s ctx ->
//            // TODO: OUCH!!! Was ist da los - wieso bekomme ich das nicht besser hin?
//            ctx.Parent :?> 'n,None

module VideApp =
    let inline doCreate appCtor (host: #ContentView) (content: Vide<'v,'s,MauiContext>) onEvaluated =
        let content = RenderRetC1Builder((fun _ -> host), fun _ -> Keep) { content }
        let ctxCtor = fun eval -> MauiContext(host, eval, MauiDocument())
        appCtor content ctxCtor onEvaluated
    let createMaui host content onEvaluated =
        doCreate VideApp.create host content onEvaluated
    let createMauiWithObjState host content onEvaluated =
        doCreate VideApp.createWithUntypedState host content onEvaluated

[<AutoOpen>]
module Defaults =
    let vide = ComponentRetCnBuilder()
