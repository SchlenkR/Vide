module UseCases.TodoList

open System
open Avalonia
open Avalonia.Layout
open Avalonia.Media
open Vide

open type Vide.AvaloniaControls

// Some defaults first...
let Card = Grid.Margin(Thickness 10.0)
let H1 = TextBlock.onInit(fun tb ->
    // TODO: Since the API is currently not auto-generated and far from complete,
    // 'onInit' is used to gain direct access to the Avalonia Control for
    // setting some defaults.
    tb.node.Margin <- Thickness(0.0, 12.0, 0.0, 24.0)
    tb.node.FontSize <- 28.0
    tb.node.FontWeight <- FontWeight.Bold
    )
let Button = Button.Margin(Thickness 5.0)
let TextBlock = TextBlock.Margin(Thickness 5.0)
let TextBox = TextBox.Margin(Thickness 5.0)
let DockPanel = DockPanel.LastChildFill(true)


// ...here we go

type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }

let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    DockPanel {
        
        H1
            .HorizontalAlignment(HorizontalAlignment.Center)
            .DockPanel().Dock(Controls.Dock.Top)
            .Text("TODO List")
        
        DockPanel.DockPanel().Dock(Controls.Dock.Bottom) {
            let! itemName = Vide.ofMutable ""

            Button
                .DockPanel().Dock(Controls.Dock.Right)
                .Margin(Thickness 0.0)
                .IsEnabled(String.IsNullOrWhiteSpace(itemName.Value) |> not)
                .Click(fun _ ->
                    let newItem = { name = itemName.Value; isDone = false }
                    do setItems (newItem :: todoList.Value.items)
                    do itemName.Reset()) { 
                    "Add Item" 
                }
            TextBox.bind(itemName)
        }

        VStack {
            for item in todoList.Value.items do
                HStack {
                    Button
                        .IsEnabled(item.isDone)
                        .Click(fun _ -> setItems (todoList.Value.items |> List.except [item])) 
                        {
                            "Remove"
                        }
                    CheckBox.bind(item.isDone, fun value -> item.isDone <- value)
                    item.name
                }
        }
    }
}
