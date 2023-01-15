module TodoList

open Vide
open type Html

type TodoItem =
    {
        name: string
    }

let addItemsComponent = vide {
    let! items = Mutable.ofValue ([] : TodoItem list)

    div.className("field has-addons") {
        p.className("control is-expanded") {
            input.className("input").type'("text") {
                "Hello World"
            }
        }
        p.className("control") {
            a
                .className("button is-info")
                .onclick(fun _ _ -> items := )
                {
                    "Add Item"
                }
        }
    }
}

let view = vide {
    h1.className("title") { "TODO List" }

    
    hr
    button.className("button is-danger") { "Reset List" }
}
