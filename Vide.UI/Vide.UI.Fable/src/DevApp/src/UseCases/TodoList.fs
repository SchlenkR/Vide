module UseCases.TodoList

open System
open Vide
open type Vide.Html


// Document:
// - here: mutable model (doesn't have to be that way)
// - "Vide.ofMutable": This also doesn't have to be that way,
//   but it's an ideomatic building block of Vide (in depth: if not,
//   emitting control statements like in LocSta would be necessary).


type TodoList = { items: TodoItem list }
and TodoItem = { name: string; mutable isDone: bool }
    
let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    h1.class'("title") { "TODO List" }
    div {
        p {
            let! itemName = Vide.ofMutable ""

            input.bind(itemName)
            button
                .disabled(String.IsNullOrWhiteSpace(itemName.Value))
                .onclick(fun _ ->
                    let newItem = { name = itemName.Value; isDone = false }
                    do setItems (newItem :: todoList.Value.items)
                    do itemName.Reset()) { 
                    "Add Item" 
                }
        }
    }
    div {
        for item in todoList.Value.items |> For.selfKeyed do
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

