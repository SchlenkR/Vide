open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Vide

type App() =
    inherit Application()
    
    let mkWindow () =
        let host = ContentControl()
        let window = Window(
            Background = Media.Brushes.DarkSlateBlue,
            Content = host
        )
        let _ = VideApp.ForHost(host).CreateAndStart(Demos.simpleVideDemo)
        window
    
    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- mkWindow ()
        | _ -> failwith "Unexpected ApplicationLifetime"


AppBuilder
    .Configure<App>()
    .UsePlatformDetect()
    .UseSkia()
    .StartWithClassicDesktopLifetime([||])
|> ignore
