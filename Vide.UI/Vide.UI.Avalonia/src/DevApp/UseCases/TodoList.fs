module UseCases.TodoList

open System
open Vide
open Vide.UI.Avalonia
open type Vide.UI.Avalonia.Controls
open type Vide.UI.Avalonia.AvaloniaControlsDefaults

type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }

let view = vide {
    let! todoList = 
        { 
            items = [
                { name = "Write Vide docu"; isDone = false }
                { name = "Cook new ramen broth"; isDone = false }
                { name = "Stuff that's already done"; isDone = true }
                { name = "Auto-gen Vide Avalonia API"; isDone = false }
                { name = "Wrap this list in ScrollViewer"; isDone = false }
            ] 
        }
        |> Vide.ofMutable
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    DockPanel.Margin(4) {
        H1
            .HorizontalAlignment(HorizontalAlignment.Center)
            .DockPanel().Dock(Dock.Top)
            .Text("My TODO List")
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
                    CheckBox
                        .bind(item.isDone, fun value -> item.isDone <- value)
                    TextBlock
                        .VerticalAlignment(VerticalAlignment.Center)
                        .TextTrimming(TextTrimming.CharacterEllipsis)
                        .Text(item.name)
                }
        }
    }
}
