
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

namespace Vide.Avalonia

open System
open System.Runtime.CompilerServices
open Vide

type AvaloniaControl = Avalonia.Controls.Control

module ControlBuilders =

    type TextBlock() =
        inherit ContentLeafRetBuilder<Avalonia.Controls.TextBlock>()

    type TextBox() =
        inherit ContentLeafPotBuilder<string, Avalonia.Controls.TextBox>(fun node -> node.Text)

    type Button() =
        inherit ContentControlRetBuilder<Avalonia.Controls.Button>()

    type CheckBox() =
        inherit ContentControlPotBuilder<Nullable<bool>, Avalonia.Controls.CheckBox>(fun node -> node.IsChecked)

    type ScrollViewer() =
        inherit ContentControlRetBuilder<Avalonia.Controls.ScrollViewer>()

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
            = this.onEval(fun x -> if x.node.Margin <> value then x.node.Margin <- value)
        
        [<Extension>]
        static member HorizontalAlignment<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> if x.node.HorizontalAlignment <> value then x.node.HorizontalAlignment <- value)
        
        [<Extension>]
        static member VerticalAlignment<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> if x.node.VerticalAlignment <> value then x.node.VerticalAlignment <- value)
        
        [<Extension>]
        static member IsEnabled<'nb,'e,'n,'c when 'nb :> NodeBuilder<'e,'n,'c> and 'e :> Avalonia.Controls.Control>
            (this: 'nb, value)
            = this.onEval(fun x -> if x.node.IsEnabled <> value then x.node.IsEnabled <- value)
    
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
        static member Text(this: #ControlBuilders.TextBlock, value) =
            this.onEval(fun x -> if x.node.Text <> value then x.node.Text <- value)
        
        [<Extension>]
        static member TextTrimming(this: #ControlBuilders.TextBlock, value) =
            this.onEval(fun x -> if x.node.TextTrimming <> value then x.node.TextTrimming <- value)
    
        // Events
    end

[<Extension>]
type TextBoxExtensions =
    class
        // Properties
        
        [<Extension>]
        static member Text(this: #ControlBuilders.TextBox, value) =
            this.onEval(fun x -> if x.node.Text <> value then x.node.Text <- value
            )
    
        // Events
    
        [<Extension>]
        static member TextChanged(this: #ControlBuilders.TextBox, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.TextChanged.Add(wrappedHandler))
                //x.node.AddHandler(Avalonia.Controls.TextBox.TextChangedEvent,)

    end

[<Extension>]
type ButtonExtensions =
    class
        // Properties
       
        // Events
    
        [<Extension>]
        static member Click(this: #ControlBuilders.Button, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.Click.Add(wrappedHandler))
    end
        
[<Extension>]
type CheckBoxExtensions =
    class
        // Properties
        
        [<Extension>]
        static member IsChecked(this: #ControlBuilders.CheckBox, value) =
            this.onEval(fun x -> if x.node.IsChecked <> value then x.node.IsChecked <- value)
    
        // Events
    
        [<Extension>]
        static member IsCheckedChanged(this: #ControlBuilders.CheckBox, handler) =
            this.onInit(fun x ->
                let wrappedHandler = Event.handle x.node x.globalContext handler
                x.node.IsCheckedChanged.Add(wrappedHandler))
    end

[<Extension>]
type ScrollViewerExtensions =
    class
        // Properties
        
        // Events
    end
        
[<Extension>]
type StackPanelExtensions =
    class
        // Properties
        
        [<Extension>]
        static member Orientation(this: #ControlBuilders.StackPanel, value) =
            this.onEval(fun x -> if x.node.Orientation <> value then x.node.Orientation <- value)
    
        // Events
    end

[<Extension>]
type DockPanelExtensions =
    class
        // Properties
        
        [<Extension>]
        static member LastChildFill(this: #ControlBuilders.DockPanel, value) =
            this.onEval(fun x -> if x.node.LastChildFill <> value then x.node.LastChildFill <- value)
    
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


type Controls =
    static member TextBlock = ControlBuilders.TextBlock()
    static member TextBox = ControlBuilders.TextBox()
    static member Button = ControlBuilders.Button()
    static member CheckBox = ControlBuilders.CheckBox()
    static member ScrollViewer = ControlBuilders.ScrollViewer()
    static member Grid = ControlBuilders.Grid()
    static member StackPanel = ControlBuilders.StackPanel()
    static member DockPanel = ControlBuilders.DockPanel()
