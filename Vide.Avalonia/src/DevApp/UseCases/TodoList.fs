module UseCases.TodoList

open System
open Vide
open Avalonia
open Avalonia.Layout
open Avalonia.Media
open type Vide.AvaloniaControls


type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }

let Card = Grid //.Margin(Thickness 10.0)

let H1 = TextBlock.onInit(fun tb -> 
    tb.node.FontSize <- 20.0
    tb.node.FontWeight <- FontWeight.Bold
    )

let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    VStack 
        .HorizontalAlignment(HorizontalAlignment.Center) 
        .VerticalAlignment(VerticalAlignment.Center) {
        
        H1.Text("TODO List")
        
        let! itemName = Vide.ofMutable ""
        TextBox.bind(itemName)
        Button
            .IsEnabled(String.IsNullOrWhiteSpace(itemName.Value) |> not)
            .Click(fun _ ->
                let newItem = { name = itemName.Value; isDone = false }
                do setItems (newItem :: todoList.Value.items)
                do itemName.Reset()) { 
                "Add Item" 
            }

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
