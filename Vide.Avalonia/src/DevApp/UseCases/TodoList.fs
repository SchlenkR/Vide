module UseCases.TodoList

open System
open Vide
open Vide.Avalonia

open type Vide.Avalonia.Controls

// Some Defaults...
type AvaloniaControlsDefaults =
    static member H1 = TextBlock.onInit(fun x ->
        // TODO: Since the API is currently not auto-generated and far from complete,
        // 'onInit' is used to gain direct access to the Avalonia Control for
        // setting some defaults.
        x.node.Margin <- Thickness(0, 12, 0, 18)
        x.node.FontSize <- 28
        x.node.FontWeight <- FontWeight.Bold
        )
    static member DockPanel = DockPanel.LastChildFill(true)

open type AvaloniaControlsDefaults


// ...here we go

type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }

let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    DockPanel.Margin(4) {
        
        H1
            .HorizontalAlignment(HorizontalAlignment.Center)
            .DockPanel().Dock(Dock.Top)
            .Text("TODO List")
        
        DockPanel
            .Margin(4) 
            .DockPanel().Dock(Dock.Bottom) {
            let! itemName = Vide.ofMutable ""

            Button
                .DockPanel().Dock(Dock.Right)
                .Margin(Thickness 0)
                .IsEnabled(String.IsNullOrWhiteSpace(itemName.Value) |> not)
                .onInit(fun x -> x.node.IsDefault <- true)
                .Click(fun _ ->
                    let newItem = { name = itemName.Value; isDone = false }
                    do setItems (newItem :: todoList.Value.items)
                    do itemName.Reset()) { 
                    "Add Item" 
                }
            TextBox.bind(itemName)
        }

        VStack.Margin(4) {
            for item in todoList.Value.items do
                DockPanel {
                    Button
                        .IsEnabled(item.isDone)
                        .DockPanel().Dock(Dock.Right)
                        .Click(fun _ -> setItems (todoList.Value.items |> List.except [item]))
                        { "Remove" }
                    CheckBox.bind(item.isDone, fun value -> item.isDone <- value)
                    TextBlock
                        .VerticalAlignment(VerticalAlignment.Center)
                        .TextTrimming(TextTrimming.CharacterEllipsis)
                        .Text(item.name)
                }
        }
    }
}
