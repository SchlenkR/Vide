namespace Vide.UI.Avalonia

open System
open System.Runtime.CompilerServices
open Vide
open Vide.UI.Avalonia

open type Vide.UI.Avalonia.Controls

[<AutoOpen>]
module AvaloniaControlsStaticMemberExtensions =
    type Vide.UI.Avalonia.Controls with
        static member HStack = StackPanel.Orientation(Avalonia.Layout.Orientation.Horizontal)
        static member VStack = StackPanel.Orientation(Avalonia.Layout.Orientation.Vertical)

module Nullable =
    let defaultValue v (n: Nullable<_>) =
        if n.HasValue then n.Value else v

[<Extension>]
type BindExtensions =

    [<Extension>]
    static member BindIsChecked(this: CheckBoxBuilder, value: MutableValue<Nullable<bool>>) =
        this
            .IsChecked(value.Value)
            .IsCheckedChanged(fun x -> value.Value <- x.node.IsChecked)

    [<Extension>]
    static member BindIsChecked(this: CheckBoxBuilder, value: Nullable<bool>, setter: Nullable<bool> -> unit) =
        this
            .IsChecked(value)
            .IsCheckedChanged(fun x -> setter(x.node.IsChecked))

    [<Extension>]
    static member BindIsChecked(this: CheckBoxBuilder, value: MutableValue<bool>) =
        this
            .IsChecked(value.Value)
            .IsCheckedChanged(fun x -> value.Value <- x.node.IsChecked |> Nullable.defaultValue false)

    [<Extension>]
    static member BindIsChecked(this: CheckBoxBuilder, value: bool, setter: bool -> unit) =
        this
            .IsChecked(value)
            .IsCheckedChanged(fun x -> setter(x.node.IsChecked |> Nullable.defaultValue false))

    [<Extension>]
    static member BindText(this: TextBoxBuilder, value: MutableValue<string>) =
        this
            .Text(value.Value)
            .TextChanged(fun args ->
                if value.Value <> args.node.Text then 
                    value.Value <- args.node.Text
                else 
                    args.requestEvaluation <- false
                do args.evt.Handled <- true
            )
    
    // Currently, don't provide this due to possibility of never-ending update cycle
    //[<Extension>]
    //static member bind(this: TextBox, value: string, setter: string -> unit) =
    //    this
    //        .Text(value)
    //        .TextInput(fun x -> setter(x.node.Text))

[<Extension>]
type ConvenienceExtensions =

    [<Extension>]
    static member inline Margin<'nb,'e,'c when
            'nb :> NodeBuilder<'e,'c>
            and 'e :> Avalonia.Controls.Control
            and 'e : (member get_Margin: unit -> Avalonia.Thickness)
            and 'e : (member set_Margin: Avalonia.Thickness -> unit)
        >
        (this: 'nb, value)
        =
        let value = Avalonia.Thickness value
        this.onEval(fun x -> if x.node.Margin <> value then x.node.Margin <- value)

    [<Extension>]
    static member inline Margin<'nb,'e,'c when
            'nb :> NodeBuilder<'e,'c>
            and 'e :> Avalonia.Controls.Control
            and 'e : (member get_Margin: unit -> Avalonia.Thickness)
            and 'e : (member set_Margin: Avalonia.Thickness -> unit)
        >
        (this: 'nb, leftRight, topBottom)
        =
        let value = Avalonia.Thickness(leftRight, topBottom)
        this.onEval(fun x -> if x.node.Margin <> value then x.node.Margin <- value)
    
    [<Extension>]
    static member inline RowDefinitions<'nb,'e,'c when 
            'nb :> NodeBuilder<'e,'c>
            and 'e :> Avalonia.Controls.Control
            and 'e : (member get_RowDefinitions: unit -> Avalonia.Controls.RowDefinitions)
            and 'e : (member set_RowDefinitions: Avalonia.Controls.RowDefinitions -> unit)
        >
        (this: 'nb, value: string) =
            // TODO: This is really dirty in the way that ToString is no guaranteed representation of the value.
            this.onEval(fun x ->
                if x.node.get_RowDefinitions().ToString() <> value
                then x.node.set_RowDefinitions(Avalonia.Controls.RowDefinitions(value)))
    
    [<Extension>]
    static member inline ColumnDefinitions<'nb,'e,'c when 
            'nb :> NodeBuilder<'e,'c>
            and 'e :> Avalonia.Controls.Control
            and 'e : (member get_ColumnDefinitions: unit -> Avalonia.Controls.ColumnDefinitions)
            and 'e : (member set_ColumnDefinitions: Avalonia.Controls.ColumnDefinitions -> unit)
        >
        (this: 'nb, value: string) =
            // TODO: This is really dirty in the way that ToString is no guaranteed representation of the value.
            this.onEval(fun x ->
                if x.node.get_ColumnDefinitions().ToString() <> value
                then x.node.set_ColumnDefinitions(Avalonia.Controls.ColumnDefinitions(value)))



// Define some Aliases for convenience
type Dock = Avalonia.Controls.Dock
type TextTrimming = Avalonia.Media.TextTrimming
type FontWeight = Avalonia.Media.FontWeight
type Thickness = Avalonia.Thickness
type HA = Avalonia.Layout.HorizontalAlignment
type VA = Avalonia.Layout.VerticalAlignment
type Wrap = Avalonia.Media.TextWrapping
type Orientation = Avalonia.Layout.Orientation


// Some Defaults...
type AvaloniaControlsDefaults =
    static member H1 =
        TextBlock.onInit(fun x ->
            // TODO: Since the API is currently not auto-generated and far from complete,
            // 'onInit' is used to gain direct access to the Avalonia Control for
            // setting some defaults.
            x.node.Margin <- Thickness(0, 12, 0, 18)
            x.node.FontSize <- 28
            x.node.FontWeight <- FontWeight.Bold
        )
    static member DockPanel =
        DockPanel.LastChildFill(true)
