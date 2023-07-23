module DemoApp.TodoList

open System
open Vide
open type Vide.Html


// Document:
// - here: mutable model (doesn't have to be that way)
// - "Vide.ofMutable": This also doesn't have to be that way,
//   but it's an ideomatic building block of Vide (in depth: if not,
//   emitting control statements like in LocSta would be necessary).

type TodoItem = { name: string; mutable isDone: bool }
type TodoList = { items: TodoItem list }
    
let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
        
    h1.class'("title") { "TODO List" }
    div {
        let! itemName = Vide.ofMutable ""
    
        p {
            let addItem () =
                let newItem = { name = itemName.Value; isDone = false }
                do
                    todoList.Value <- { todoList.Value with items = newItem :: todoList.Value.items }
                    itemName.Reset()
                
            input
                .type'("text")
                .value(itemName.Value)
                .oninput(fun x -> itemName.Value <- x.node.value)
                
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
                p { item.name }
                input
                    .type'("checkbox")
                    .checked'(item.isDone)
                    .oninput(fun x -> item.isDone <- x.node.``checked``)
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

