
// #r "nuget: Vide.UI.Avalonia.Interactive"
#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"
#r "nuget: Vide.UI.Avalonia, 0.0.24"
#load "../Vide.UI.Avalonia.Interactive/fsi.fs"

open Vide.UI.Avalonia
open Vide.UI.Avalonia.Interactive.Dynamic
Interactive.guardInit ()

// ^ -------------------------------------------------------------
// |_ This is the boilerplate to make the sample work in fsi.
//    Evaluate this _once and separate_ from the rest of the sample.
// ---------------------------------------------------------------



// ---------------------------------------------------------------
// Here starts the actual sample ...
// ---------------------------------------------------------------

open System
open Vide
open type Vide.UI.Avalonia.Controls
open type Vide.UI.Avalonia.AvaloniaControlsDefaults

type TodoList = { items: TodoItem list }
and TodoItem = { text: string; mutable isDone: bool; key: int }

let view = vide {
    let! todoList = ofMutable {
        { 
            items = 
                [
                    {| text = "Write Vide docu"; isDone = false |}
                    {| text = "Cook new ramen broth"; isDone = false |}
                    {| text = "Stuff that's already done"; isDone = true |}
                    {| text = "Auto-gen Vide Avalonia API"; isDone = true |}
                    {| text = "Fix a Trulla C# codegen bug"; isDone = false |}
                    {| text = "Make a Trulla version for node"; isDone = false |}
                    {| text = "Write a LSP for Trulla templates"; isDone = false |}
                ]
                |> List.mapi (fun i x -> { text = x.text; isDone = x.isDone ;key = i })
        }
    }
    
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    DockPanel.Margin(4) {
        H1
            .HorizontalAlignment(HorizontalAlignment.Center)
            .DockPanel().Dock(Dock.Top)
            .Text("My TODO List")
        DockPanel
            .Margin(4) 
            .DockPanel().Dock(Dock.Bottom) {
            
            let! itemName = ofMutable {""}

            Button
                .DockPanel().Dock(Dock.Right)
                .Margin(0)
                .IsEnabled(String.IsNullOrWhiteSpace(itemName.Value) |> not)
                .onInit(fun x -> x.node.IsDefault <- true)
                .Click(fun _ ->
                    let nextId = 
                        match  todoList.Value.items |> List.map (fun x -> x.key) |> List.sortDescending with
                        | [] -> 0
                        | x::_ -> x + 1
                    let newItem = { text = itemName.Value; isDone = false; key = nextId }
                    do setItems (newItem :: todoList.Value.items)
                    do itemName.Reset()) { 
                        "Add Item"
                }
            TextBox.bind(itemName)
        }

        VStack.Margin(4) {
            for item in todoList.Value.items do
                DockPanel {
                    Button
                        .IsEnabled(item.isDone)
                        .DockPanel().Dock(Dock.Right)
                        .Click(fun _ -> setItems (todoList.Value.items |> List.except [item]))
                        { "Remove" }
                    CheckBox
                        .bind(item.isDone, fun value -> item.isDone <- value)
                    TextBlock
                        .VerticalAlignment(VerticalAlignment.Center)
                        .TextTrimming(TextTrimming.CharacterEllipsis)
                        .Text(item.text)
                }
        }
    }
}


// The view defined above is shown in a new window.
let videApp = 
    Interactive.createWindowAndShow 300. 500.
    |> Interactive.showView view

let printState (s: obj) =
    let rec prec indentation (s: obj) =
        try
            if s = null then
                printfn "%snull" indentation
            elif s.GetType().FullName.StartsWith("Microsoft.FSharp.Core.FSharpOption`1[[System.Tuple`2") then
                try
                    let tuple = s?Value
                    let item1 = tuple?Item1
                    let item2 = tuple?Item2
                    printfn "%A" item1
                    prec (indentation + "  ") item2
                with _ ->
                    printfn "%sNone" indentation
            else
                printfn "%s%s" indentation (s.ToString())
        with e ->
            printfn "%s%s" indentation (e.ToString())
    prec "" s

videApp.OnEvaluated(fun v s -> printfn "EVAL")
