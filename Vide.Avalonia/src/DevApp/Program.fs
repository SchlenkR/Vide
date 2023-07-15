open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Vide

type App() =
    inherit Application()
    
    //let demo = UseCases.GettingStarted.counter
    let demo = UseCases.TodoList.view
    
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
                    Content = host
                )
                let _ = VideApp.ForHost(host).CreateAndStart(demo)
                window
        | _ -> failwith "Unexpected ApplicationLifetime"

AppBuilder
    .Configure<App>()
    .UsePlatformDetect()
    .UseSkia()
    .StartWithClassicDesktopLifetime([||])
|> ignore
