open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Vide
open Vide.UI.Avalonia

module FormFactor =
    let mobile = 390.0, 644.0
    let desktop = 1200.0, 700.0

type App() =
    inherit Application()

    //let demo = UseCases.GettingStarted.counter, FormFactor.desktop
    let demo = UseCases.TodoList.view, FormFactor.mobile
    //let demo = UseCases.EvalDebugger.view, FormFactor.mobile
    
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
                    Width = fst (snd demo),
                    Height = snd (snd demo),
                    WindowStartupLocation = WindowStartupLocation.CenterOwner
                )
                let app = VideApp.ForHost(host).CreateAndStart(fst demo)
                do app.OnEvaluated(fun v s ->
                    ()
                    // System.Diagnostics.Debug.WriteLine($"Eval DONE ({app.EvaluationManager.EvaluationCount} cycles)")
                )
                window
        | _ -> failwith "Unexpected ApplicationLifetime"

AppBuilder
    .Configure<App>()
    .UsePlatformDetect()
    .UseSkia()
    .StartWithClassicDesktopLifetime([||])
|> ignore
