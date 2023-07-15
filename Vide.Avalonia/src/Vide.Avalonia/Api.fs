
//------------------------------------------------------------------------------
// <NOT YET auto-generated>
//
//     This code was NOT YET  auto generated.
//
//     This file will be auto generated, but is it currently
//     hand-crafted. It follows the unwritten guidelines of
//     "HowTo Craft a Vide UI API" ;)
//
// </NOT YET auto-generated>
//------------------------------------------------------------------------------

namespace Vide

open System
open System.Runtime.CompilerServices
open Vide

type AvaloniaControl = Avalonia.Controls.Control

module AvaloniaControlBuilders =

    type TextBlock() =
        inherit ContentLeafRetBuilder<Avalonia.Controls.TextBlock>()

    type TextBox() =
        inherit ContentLeafPotBuilder<string, Avalonia.Controls.TextBox>(fun node -> node.Text)

    type Button() =
        inherit ContentControlRetBuilder<Avalonia.Controls.Button>()

    type CheckBox() =
        inherit ContentControlPotBuilder<Nullable<bool>, Avalonia.Controls.CheckBox>(fun node -> node.IsChecked)

    type Grid() =
        inherit PanelRetBuilder<Avalonia.Controls.Grid>()

    type StackPanel() =
        inherit PanelRetBuilder<Avalonia.Controls.StackPanel>()

    type DockPanel() =
        inherit PanelRetBuilder<Avalonia.Controls.DockPanel>()


// -------------------------------


[<Extension>]
type NodeBuilderExtensions =
    class
        // Properties
        
        [<Extension>]
        static member inline Margin<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> x.node.Margin <- value)
        
        [<Extension>]
        static member HorizontalAlignment<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> x.node.HorizontalAlignment <- value)
        
        [<Extension>]
        static member VerticalAlignment<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> x.node.VerticalAlignment <- value)
        
        [<Extension>]
        static member IsEnabled<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> x.node.IsEnabled <- value)
    
        // Events
    end

[<Extension>]
type ContentLeafPotBuilderExtensions =
    class
        // Properties
        // Events
    end

[<Extension>]
type ContentControlRetBuilderExtensions =
    class
        // Properties
        // Events
    end

[<Extension>]
type PanelRetBuilderExtensions =
    class
        // Properties
        // Events
    end


// -------------------------------


[<Extension>]
type TextBlockExtensions =
    class
        // Properties
        
        [<Extension>]
        static member Text(this: #AvaloniaControlBuilders.TextBlock, value) =
            this.onEval(fun x -> x.node.Text <- value)
    
        // Events
    end

[<Extension>]
type TextBoxExtensions =
    class
        // Properties
        
        [<Extension>]
        static member Text(this: #AvaloniaControlBuilders.TextBox, value) =
            this.onEval(fun x -> if x.node.Text <> value then x.node.Text <- value)
    
        // Events
    
        [<Extension>]
        static member TextChanged(this: #AvaloniaControlBuilders.TextBox, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.TextChanged.Add(wrappedHandler))

    end

[<Extension>]
type ButtonExtensions =
    class
        // Properties
       
        // Events
    
        [<Extension>]
        static member Click(this: #AvaloniaControlBuilders.Button, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.Click.Add(wrappedHandler))
    end
        

[<Extension>]
type CheckBoxExtensions =
    class
        // Properties
        
        [<Extension>]
        static member IsChecked(this: #AvaloniaControlBuilders.CheckBox, value) =
            this.onEval(fun x -> x.node.IsChecked <- value)
    
        // Events
    
        [<Extension>]
        static member IsCheckedChanged(this: #AvaloniaControlBuilders.CheckBox, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.IsCheckedChanged.Add(wrappedHandler))
    end
        
[<Extension>]
type StackPanelExtensions =
    class
        // Properties
        
        [<Extension>]
        static member Orientation(this: #AvaloniaControlBuilders.StackPanel, value) =
            this.onEval(fun x -> x.node.Orientation <- value)
    
        // Events
    end

[<Extension>]
type DockPanelExtensions =
    class
        // Properties
        
        [<Extension>]
        static member LastChildFill(this: #AvaloniaControlBuilders.DockPanel, value) =
            this.onEval(fun x -> x.node.LastChildFill <- value)
    
        // Events
    end


// -------------------------------

module AttachedProperties =
    type DockPanel<'nb,'e
            when 'nb :> NodeBuilder<'e,AvaloniaControl,AvaloniaContext>
            and 'e :> AvaloniaControl>
        = { target: 'nb }

[<Extension>]
type DockPanelAttachedProperties =
    class
        // Entry

        [<Extension>]
        static member inline DockPanel(this: #NodeBuilder<_,AvaloniaControl,AvaloniaContext>)  =
            { AttachedProperties.DockPanel.target = this }

        // Properties

        [<Extension>]
        static member inline Dock(this: AttachedProperties.DockPanel<_,_>, value) =
            this.target.onEval(fun x -> Avalonia.Controls.DockPanel.SetDock(x.node, value))

    end


// -------------------------------


type AvaloniaControls =
    static member inline TextBlock = AvaloniaControlBuilders.TextBlock()
    static member inline TextBox = AvaloniaControlBuilders.TextBox()
    static member inline Button = AvaloniaControlBuilders.Button()
    static member inline CheckBox = AvaloniaControlBuilders.CheckBox()
    static member inline Grid = AvaloniaControlBuilders.Grid()
    static member inline StackPanel = AvaloniaControlBuilders.StackPanel()
    static member inline DockPanel = AvaloniaControlBuilders.DockPanel()
