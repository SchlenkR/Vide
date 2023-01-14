module TodoList

open Vide
open type Html

let view = vide {
    h1.className("title") { "TODO List" }

    input.className("input").type'("text") {
        "Hello World"
    }
    
    hr
    button.className("button is-danger") { "Reset List" }
}
