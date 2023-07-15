namespace Vide

open System
open System.Runtime.CompilerServices
open Vide
open Vide.AvaloniaControlBuilders

[<AutoOpen>]
module AvaloniaControlsStaticMemberExtensions =
    type AvaloniaControls with
        static member inline HStack = StackPanel().Orientation(Avalonia.Layout.Orientation.Horizontal)
        static member inline VStack = StackPanel().Orientation(Avalonia.Layout.Orientation.Vertical)

module Nullable =
    let defaultValue v (n: Nullable<_>) =
        if n.HasValue then n.Value else v

[<Extension>]
type TextBoxBindExtensions =
    
    [<Extension>]
    static member bind(this: TextBox, value: MutableValue<string>) =
        this
            .Text(value.Value)
            .TextChanged(fun args ->
                if value.Value <> args.node.Text
                then value.Value <- args.node.Text
                else args.requestEvaluation <- false
            )
    
    // Currently, don't provide this due to possibility of never-ending update cycle
    //[<Extension>]
    //static member bind(this: TextBox, value: string, setter: string -> unit) =
    //    this
    //        .Text(value)
    //        .TextInput(fun x -> setter(x.node.Text))
    

[<Extension>]
type CheckBoxBindExtensions =

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
    