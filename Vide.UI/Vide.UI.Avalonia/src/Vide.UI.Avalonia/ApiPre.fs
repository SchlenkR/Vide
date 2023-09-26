namespace Vide.UI.Avalonia

open Avalonia.Controls
open Vide
open Vide.WpfishModel

/// A builder that works on controls that have no content and don't return a value; e.g. Label.
type ContentLeafRetBuilder<'e when 'e :> Control and 'e : (new: unit -> 'e)>() =
    inherit RenderRetC0Builder<'e>(BuilderHelper.createNode)

/// A builder that works on controls that have no content and can return a value; e.g. TextBox.
type ContentLeafPotBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createResultVal) =
    inherit RenderPotC0Builder<'v,'e>(BuilderHelper.createNode, createResultVal)

/// A builder that works on a single content that don't return a value; e.g. Button.
type ContentControlRetBuilder<'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>() =
    inherit RenderRetC1Builder<'e>(BuilderHelper.createNode)

/// A builder that works on a single content that can return a value; e.g. CheckBox.
type ContentControlPotBuilder<'v,'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(createResultVal) =
    inherit RenderPotC1Builder<'v,'e>(BuilderHelper.createNode, createResultVal)

/// A builder that works on many children; e.g. StackPanel
type PanelRetBuilder<'e when 'e :> Panel and 'e :> Panel and 'e : (new: unit -> 'e)>() =
    inherit RenderRetCnBuilder<'e>(BuilderHelper.createNode)
