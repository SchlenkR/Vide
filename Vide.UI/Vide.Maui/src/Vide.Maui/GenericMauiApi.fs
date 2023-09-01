namespace Vide.GenericMauiApi

open System
open System.Collections.Generic
open Microsoft.Maui
open Microsoft.Maui.Controls

open Vide
open Vide.WpfishModel

module Helper =
    let registerCtor (this: NodeBuilder<_,_,_>) onEval = 
        onEval |> Option.iter (fun onEval -> this.PreEvalModifiers.Add(fun ctx -> onEval ctx.node))

/// A builder that works on IView (no content; e.g. Maui.Controls.Label)
type V<'e when 'e :> IView and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetC0Builder<'e>(BuilderHelper.createNode)
    do Helper.registerCtor this onEval
        
/// A builder that works on a ContentView (single content; e.g. Maui.Controls.ContentView)
type C<'e when 'e :> ContentView and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetC1Builder<'e>(BuilderHelper.createNode)
    do Helper.registerCtor this onEval

/// A builder that works on IView collections (many children; e.g. Maui.Controls.StackLayout)
type P<'e when 'e :> ICollection<IView> and 'e :> IView and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetCnBuilder<'e>(BuilderHelper.createNode)
    do Helper.registerCtor this onEval
