#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"
#r "nuget: Vide.UI.Avalonia, 0.0.23"


// EVAL ONLY ONCE PER FSI SESSION


// ---------------------------------------------------------------
// ...some gibberish to make the fiddle work
// ---------------------------------------------------------------
module VideApp =
    open Avalonia
    open Avalonia.Controls.ApplicationLifetimes
    open Avalonia.Controls
    open Avalonia.Themes.Fluent
    open Vide
    open Vide.UI.Avalonia

    type App<'v,'s>(content: Vide<'v,'s,HostContext<AvaloniaContext>>, width: float, height: float) =
        inherit Application()
        override this.Initialize() =
            this.Styles.Add(FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                desktopLifetime.MainWindow <-
                    let host = ContentControl()
                    let window = Window(
                        Background = Media.Brushes.DarkSlateBlue,
                        Content = host,
                        Width = width,
                        Height = height,
                        WindowStartupLocation = WindowStartupLocation.CenterOwner
                    )
                    let app = VideApp.ForHost(host).CreateAndStartWithUntypedState(content)
                    do app.OnEvaluated(fun v s ->
                        ()
                        // System.Diagnostics.Debug.WriteLine($"Eval DONE ({app.EvaluationManager.EvaluationCount} cycles)")
                    )
                    window
            | _ -> failwith "Unexpected ApplicationLifetime"

    let start content (size: {| width: float; height: float |}) =
        AppBuilder
            .Configure(fun () -> App(content, size.width, size.height))
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime([||])
        |> ignore


// ---------------------------------------------------------------
// here starts the actual sample
// ---------------------------------------------------------------

open System
open Vide
open Vide.UI.Avalonia
open type Vide.UI.Avalonia.Controls
open type Vide.UI.Avalonia.AvaloniaControlsDefaults

type TodoList = { items: TodoItem list }
and TodoItem = { text: string; mutable isDone: bool; key: int }

let view = vide {
    let! todoList = 
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
        |> Vide.ofMutable
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    DockPanel.Margin(4) {
        H1
            .HorizontalAlignment(HorizontalAlignment.Center)
            .DockPanel().Dock(Dock.Top)
            .Text("My TODO List")
        DockPanel
            .Margin(4) 
            .DockPanel().Dock(Dock.Bottom) {
            
            let! itemName = Vide.ofMutable ""

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


VideApp.start view {| width = 390.0; height = 644.0 |}
