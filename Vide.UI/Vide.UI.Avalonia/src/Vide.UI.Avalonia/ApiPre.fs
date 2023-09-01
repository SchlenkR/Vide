namespace Vide.UI.Avalonia

open Avalonia.Controls
open Vide
open Vide.WpfishModel

/// A builder that works on controls that have no content; e.g. Label.
type ContentLeafRetBuilder<'e when 'e :> Control and 'e : (new: unit -> 'e)>() =
    inherit RenderRetC0Builder<'e>(BuilderHelper.createNode)

/// A builder that works on controls that have no content; e.g. Label.
type ContentLeafPotBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createResultVal) =
    inherit RenderPotC0Builder<'v,'e>(BuilderHelper.createNode, createResultVal)

/// A builder that works on a single content
type ContentControlRetBuilder<'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>() =
    inherit RenderRetC1Builder<'e>(BuilderHelper.createNode)

/// A builder that works on a single content
type ContentControlPotBuilder<'v,'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(createResultVal) =
    inherit RenderPotC1Builder<'v,'e>(BuilderHelper.createNode, createResultVal)

/// A builder that works on many children; e.g. StackPanel
type PanelRetBuilder<'e when 'e :> Panel and 'e :> Panel and 'e : (new: unit -> 'e)>() =
    inherit RenderRetCnBuilder<'e>(BuilderHelper.createNode)
