namespace ClassLibrary1

module Startup =

    open System
    open System.Windows
    open System.Windows.Media
    
    [<STAThread; EntryPoint>]
    let main args =
        let app = Application()
        let window = Window(
            Background = Brushes.Lime,
            Width = 600,
            Height = 500
        )
        app.MainWindow <- window
        app.Startup.Add <| fun _ ->
            do window.Show()
            LocSta.App.startWpf window
        app.Run() |> ignore
        0
