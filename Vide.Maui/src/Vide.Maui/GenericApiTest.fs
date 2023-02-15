namespace Vide.Maui.GenericApiTest

open System
open System.Collections.Generic
open Microsoft.Maui
open Microsoft.Maui.Controls
open Vide
open Vide.WpfishModel
open Vide.Maui

module Helper =
    let registerCtor (this: RenderBaseBuilder<_,_>) onEval = 
        onEval |> Option.iter (fun onEval -> this.ModifierContext.EvalModifiers.Add(fun ctx -> onEval ctx.node))
    let createNode (ctx: MauiContext) = ctx.AddElement()
    let checkNode<'n> (node: IView) = BuilderHelper.checkNode(typeof<'n>.FullName, node.GetType().FullName)

/// A builder that works on IView (no content; e.g. Maui.Controls.Label)
type V<'n when 'n :> IView and 'n : (new: unit -> 'n)>(?onEval) as this =
    inherit RenderRetC0Builder<'n>(Helper.createNode, Helper.checkNode<'n>)
    do Helper.registerCtor this onEval
        
/// A builder that works on a ContentView (single content; e.g. Maui.Controls.ContentView)
type C<'n when 'n :> ContentView and 'n : (new: unit -> 'n)>(?onEval) as this =
    inherit RenderRetC1Builder<'n>(Helper.createNode, Helper.checkNode<'n>)
    do Helper.registerCtor this onEval

/// A builder that works on IView collections (many children; e.g. Maui.Controls.StackLayout)
type P<'n when 'n :> ICollection<IView> and 'n :> IView and 'n : (new: unit -> 'n)>(?onEval) as this =
    inherit RenderRetCnBuilder<'n>(Helper.createNode, Helper.checkNode<'n>)
    do Helper.registerCtor this onEval
