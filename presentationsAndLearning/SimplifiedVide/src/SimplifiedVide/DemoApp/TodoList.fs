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
type TodoList = { items: ResizeArray<TodoItem> }
    
let view = vide {
    let! todoList = Vide.ofMutable { items = ResizeArray() }
        
    h1.class'("title") { "My TODO List" }
    div.id("addItemPanel") {
        let! itemName = Vide.ofMutable ""
        let addItem () =
            let newItem = { name = itemName.Value; isDone = false }
            do todoList.Value.items.Add(newItem)
            do itemName.Reset()
        input
            .class'("itemName")
            .type'("text")
            .value(itemName.Value)
            .oninput(fun x -> itemName.Value <- x.node.value)
        button
            .class'("newItem")
            .disabled(String.IsNullOrWhiteSpace(itemName.Value))
            .onclick(fun _ -> addItem()) 
            { 
                "Add Item" 
            }
    }
    div.id("itemsPanel") {
        for item in todoList.Value.items do
            let removeItem () = do todoList.Value.items.Remove(item) |> ignore
            div.class'("item") {
                input
                    .class'("itemDone")
                    .type'("checkbox")
                    .checked'(item.isDone)
                    .oninput(fun x -> item.isDone <- x.node.``checked``)
                span { item.name }
                button
                    .class'("removeItem")
                    .disabled(not item.isDone)
                    .onclick(fun _ -> removeItem ()) 
                    {
                        "Remove" 
                    }
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

