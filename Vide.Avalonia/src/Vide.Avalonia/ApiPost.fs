namespace Vide.Avalonia

open System
open System.Runtime.CompilerServices
open Vide
open Vide.Avalonia
open Vide.Avalonia.ControlBuilders

open type Vide.Avalonia.Controls

[<AutoOpen>]
module AvaloniaControlsStaticMemberExtensions =
    type Vide.Avalonia.Controls with
        static member HStack = StackPanel.Orientation(Avalonia.Layout.Orientation.Horizontal)
        static member VStack = StackPanel.Orientation(Avalonia.Layout.Orientation.Vertical)

module Nullable =
    let defaultValue v (n: Nullable<_>) =
        if n.HasValue then n.Value else v

[<Extension>]
type BindExtensions =

    [<Extension>]
    static member bind(this: CheckBox, value: MutableValue<Nullable<bool>>) =
        this
            .IsChecked(value.Value)
            .IsCheckedChanged(fun x -> value.Value <- x.node.IsChecked)
    
    [<Extension>]
    static member bind(this: CheckBox, value: Nullable<bool>, setter: Nullable<bool> -> unit) =
        this
            .IsChecked(value)
            .IsCheckedChanged(fun x -> setter(x.node.IsChecked))
    
    [<Extension>]
    static member bind(this: CheckBox, value: MutableValue<bool>) =
        this
            .IsChecked(value.Value)
            .IsCheckedChanged(fun x -> value.Value <- x.node.IsChecked |> Nullable.defaultValue false)
    
    [<Extension>]
    static member bind(this: CheckBox, value: bool, setter: bool -> unit) =
        this
            .IsChecked(value)
            .IsCheckedChanged(fun x -> setter(x.node.IsChecked |> Nullable.defaultValue false))

        
    [<Extension>]
    static member bind(this: TextBox, value: MutableValue<string>) =
        let x = "xxxx"
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
    static member inline Margin<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, value) = this.Margin(Avalonia.Thickness(value))

    [<Extension>]
    static member inline Margin<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, leftRight, topBottom) = this.Margin(Avalonia.Thickness(leftRight, topBottom))

    [<Extension>]
    static member inline MarginLeft<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, value) = this.Margin(Avalonia.Thickness(value, 0, 0, 0))

    [<Extension>]
    static member inline MarginTop<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, value) = this.Margin(Avalonia.Thickness(0, value, 0, 0))

    [<Extension>]
    static member inline MarginRight<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, value) = this.Margin(Avalonia.Thickness(0, 0, value, 0))

    [<Extension>]
    static member inline MarginBottom<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
        (this: 'nb, value) = this.Margin(Avalonia.Thickness(0, 0, 0, value))

    
// Define some Aliases for convenience
type Dock = Avalonia.Controls.Dock
type TextTrimming = Avalonia.Media.TextTrimming
type FontWeight = Avalonia.Media.FontWeight
type Thickness = Avalonia.Thickness
type HorizontalAlignment = Avalonia.Layout.HorizontalAlignment
type VerticalAlignment = Avalonia.Layout.VerticalAlignment
