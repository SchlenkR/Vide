module UseCases.VideShowcase

open System
open Vide
open type Vide.Html


type TodoItem = { name: string; mutable isDone: bool }
type TodoList = { items: TodoItem list }
    
let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    h1.class'("title") { "TODO List" }
    div {
        let! itemName = Vide.ofMutable ""
    
        p {
            let addItem () =
                let newItem = { name = itemName.Value; isDone = false }
                do
                    setItems (newItem :: todoList.Value.items)
                    itemName.Reset()
                
            input.bind(itemName)
            button
                .disabled(String.IsNullOrWhiteSpace(itemName.Value))
                .onclick(fun _ -> addItem()) 
                { 
                    "Add Item" 
                }
        }
    }
    div {
        for item in todoList.Value.items do
            div.class'("flex-row") {
                input.bind(item.isDone, fun value -> item.isDone <- value)
                button
                    .disabled(not item.isDone)
                    .onclick(fun _ -> setItems (todoList.Value.items |> List.except [item])) {
                    "Remove"
                }
                p { item.name }
            }
    }
}
