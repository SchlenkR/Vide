
#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"
#r "nuget: Vide.UI.Avalonia, 0.0.24"


[<AutoOpen>]
module VideApp =
    open Avalonia
    open Avalonia.Controls.ApplicationLifetimes
    open Avalonia.Controls
    open Avalonia.Themes.Fluent
    open Avalonia.Threading

    let mutable private host = None

    let disp (f: unit -> 'a) =
        Dispatcher.UIThread.InvokeAsync(f).GetTask()
        |> Async.AwaitTask 
        |> Async.RunSynchronously 

    type App<'v,'s>(width: float, height: float) =
        inherit Application()
        override this.Initialize() =
            this.Styles.Add(FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                desktopLifetime.MainWindow <-
                    let cc = ContentControl()
                    do host <- Some cc
                    Window(
                        Background = Media.Brushes.DarkSlateBlue,
                        Content = cc,
                        Width = width,
                        Height = height,
                        WindowStartupLocation = WindowStartupLocation.CenterOwner
                    )
            | _ -> failwith "Unexpected ApplicationLifetime"

    open FSharp.Compiler.Interactive

    fsi.EventLoop <-
        { new IEventLoop with 
            member x.Run() =
                AppBuilder
                    .Configure(fun () -> App(390.0, 644.0))
                    .UsePlatformDetect()
                    .UseSkia()
                    .StartWithClassicDesktopLifetime([||])
                    |> ignore
                false
            member x.Invoke(f) = disp f
            member x.ScheduleRestart() = ()
        }

    type Host =
        static member Instance = host.Value


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

let todoListView = vide {
    let! todoList = ofMutable {
        log "Eval"
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



VideApp.ForHost(Host.Instance).CreateAndStartWithUntypedState(todoListView) |> ignore


Host.Instance.Content <- null




let ofMutable x =
    mkVide <| fun s (ctx: HostContext<_>) ->
        let s = s |> Option.defaultWith (fun () -> printfn "NEW"; MutableValue(x, ctx.host))
        s, Some s

let x = vide {
    log "A"
    let! x = ofMutable "Value 1"
    log "B"
    H1 { x.Value }
    log "C"
    let! y = ofMutable "Value 2"
    log "D"
    H1 { y.Value }
    log "2"
}


x None 
