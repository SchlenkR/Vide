module TodoList

open Fable.Core.JS
open Vide
open type Html

type TodoItem =
    {
        name: string
    }

type TodoList =
    {
        items: TodoItem list
    }

let view = vide {
    h1.class'("title") { "TODO List" }

    let! items = Vide.ofMutable { items = [] }

    div.class'("field has-addons") {
        let! itemName = p.class'("control is-expanded") {
            let! text = input.class'("input").type'("text")
            return text.textValue
        }

        p.class'("control") {
            let addItem _ =
                let newItem = { name = itemName }
                items.Value <- { items.Value with items = newItem :: items.Value.items }
            a.class'("button is-info").onclick(addItem) { "Add Item" }
        }
    }
    
    div {
        console.log(">>>>>>>>>>>>>>>")
        for item in items.Value.items do
            console.log item.name
            div {
                p { item.name }
            }
        console.log("<<<<<<<<<<<<<<<")
    }

    
    hr
    button.class'("button is-danger") { "Reset List" }
}
