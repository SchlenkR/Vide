module UseCases.TodoList

open System
open Vide
open Avalonia
open type Vide.AvaloniaControls


type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }

let Card = Grid.Margin(Thickness 10.0)

let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    TextBlock.Text("TODO List")
    StackPanel {
        Grid {
            let! itemName = Vide.ofMutable ""

            let a = TextBox.bind(itemName)
            let b = Button
            yield a
            yield b
                // .IsEnabled(not String.IsNullOrWhiteSpace(itemName.Value)) {}
                // .Click(fun _ ->
                //     let newItem = { name = itemName.Value; isDone = false }
                //     do setItems (newItem :: todoList.Value.items)
                //     do itemName.Reset()) { 
                //     "Add Item" 
                // }
        }
    }
    VStack {
        for item in todoList.Value.items do
            Grid {
                Button
                    .IsEnabled(not item.isDone)
                    .Click(fun _ -> setItems (todoList.Value.items |> List.except [item])) 
                    {
                        "Remove"
                    }
                CheckBox //.bind(item.isDone, fun value -> item.isDone <- value)
                Card { item.name }
            }
    }
}



//module Playground =

//    let bindTest =
//        vide {
//            div.OnEval(fun x _ -> x.className <- "bam")  {
//                "I'm the OnEval div"
//            }

//            div.OnInit(fun x _ -> x.className <- "bam2") {
//                "I'm the OnInit div"
//            }
//        }

