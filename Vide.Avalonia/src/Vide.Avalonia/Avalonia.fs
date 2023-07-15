namespace Vide.Avalonia

open System
open Avalonia.Controls
open Vide
open Vide.WpfishModel

// The current basis for vide-implementations is the
// NodeModel (with NodeDocument and NodeContext), which
// actually works like a true node model (always many children).
// The node model doesn't distinguish between different container
// forms (e.g. single / multi content), so there are some casts
// needed here and runtime exceptions can occur. Anyway, content models can be modeled correctly at the API
// surface, using the Render-builders.

type AvaloniaDocument() =
    let makeEx parent child actionName =
        let childTypeName = match child with Some child -> child.GetType().Name | None -> "-"
        Exception $"Cannot perform '{actionName}' to node of type {parent.GetType().Name} (child type = {childTypeName})."
    interface INodeDocument<Control> with
        member _.EnsureChildAppended(parent, child) =
            match box parent with
            | :? Panel as panel ->
                if not (panel.Children.Contains(child)) then
                    panel.Children.Add(child)
            | :? ContentControl as cc ->
                if cc.Content <> child then
                    cc.Content <- child
            | _ -> raise <| makeEx parent (Some child) "EnsureChildAppended"
        member _.RemoveChild(parent, child) =
            match box parent with
            | :? Panel as panel -> panel.Children.Remove(child) |> ignore
            | :? ContentControl as cc -> cc.Content <- null
            | _ -> raise <| makeEx parent (Some child) "RemoveChild"
        member _.GetChildren(parent) =
            match box parent with
            | :? Panel as panel -> panel.Children |> Seq.toList
            | :? ContentControl as cc ->
                // TODO: Das ist alles sehr suboptimal hier (i.A.)
                match cc.Content with
                | :? Control as c -> [c]
                | _ -> []
            | _ -> []
        member _.ClearChildren(parent) =
            match box parent with
            | :? Panel as panel -> panel.Children.Clear()
            | :? ContentControl as cc -> cc.Content <- null
            | _ -> raise <| makeEx parent None "ClearChildren"
        member _.CreateTextNode(text) =
            let tn = new TextBlock(Text = text)
            let textNode =
                {
                    node = tn :> Control
                    getText = fun () -> tn.Text
                    setText = fun value -> tn.Text <- value
                }
            textNode
    interface IWpfishDocument<Control> with
        member _.CreateNodeOfType<'e when 'e : (new: unit -> 'e)>() =
            let e = new 'e()
            e, (box e) :?> Control

type AvaloniaContext(parent: Control) =
    inherit WpfishContext<Control>(parent, AvaloniaDocument())
    static member Create<'e when 'e :> Control>(thisNode: 'e) = AvaloniaContext(thisNode)


// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Control,AvaloniaContext>()

type RenderValC0Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotC0Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetC0Builder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode) =
    inherit RenderRetC0BaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode)

type RenderValC1Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotC1Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderPotC1BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetC1Builder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode) =
    inherit RenderRetC1BaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode)

type RenderValCnBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderPotCnBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode, createResultVal) =
    inherit RenderPotCnBaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode, createResultVal)
type RenderRetCnBuilder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, checkChildNode) =
    inherit RenderRetCnBaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.Create, createThisElement, checkChildNode)


// --------------------------------------------------
// App
// --------------------------------------------------
    
type VideApp =
    static member ForHost(host) = 
        VideAppFactory(
            (fun () -> AvaloniaContext(host)),
            (fun (ctx: AvaloniaContext) -> ctx.RemoveObsoleteChildren())
        )

[<AutoOpen>]
module TopLevels =
    let vide = ComponentRetCnBuilder()
