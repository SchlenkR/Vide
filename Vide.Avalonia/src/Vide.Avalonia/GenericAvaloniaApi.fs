namespace Vide.GenericAvaloniaApi

open Avalonia.Controls
open Vide
open Vide.WpfishModel

module Helper =
    let registerCtor (this: NodeBuilder<_,_,_>) onEval = 
        onEval |> Option.iter (fun onEval -> this.EvalModifiers.Add(fun ctx -> onEval ctx.node))
    let checkNode<'e> (node: Control) =
        BuilderHelper.checkNode(typeof<'e>.FullName, node.GetType().FullName)

/// A builder that works on controls that have no content; e.g. Label
type V<'e when 'e :> Control and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetC0Builder<'e>(BuilderHelper.createNode, Helper.checkNode<'e>)
    do Helper.registerCtor this onEval
        
/// A builder that works on a single content
type C<'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetC1Builder<'e>(BuilderHelper.createNode, Helper.checkNode<'e>)
    do Helper.registerCtor this onEval

/// A builder that works on many children; e.g. StackPanel
type P<'e when 'e :> Panel and 'e :> Panel and 'e : (new: unit -> 'e)>(?onEval) as this =
    inherit RenderRetCnBuilder<'e>(BuilderHelper.createNode, Helper.checkNode<'e>)
    do Helper.registerCtor this onEval
