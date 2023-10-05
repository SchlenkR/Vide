
#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"
#r "nuget: Vide.UI.Avalonia, 0.0.24"

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Avalonia.Threading

type App<'v,'s>() =
    inherit Application()
    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

open FSharp.Compiler.Interactive

if Application.Current = null then
    fsi.EventLoop <-
        { new IEventLoop with 
            member _.Run() =
                AppBuilder
                    .Configure(fun () -> App())
                    .UsePlatformDetect()
                    .UseSkia()
                    .StartWithClassicDesktopLifetime([||], ShutdownMode.OnExplicitShutdown)
                    |> ignore
                false
            member _.Invoke(f) =
                Dispatcher.UIThread.Invoke(System.Func<'a>(fun () -> f ()))
            member _.ScheduleRestart() = ()
        }

module VideAppWindow =
    let createAndShow width height =
        match Application.Current.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let host = ContentControl()
            let window = Window(
                Background = Media.Brushes.DarkSlateBlue,
                Content = host,
                Width = width,
                Height = height,
                WindowStartupLocation = WindowStartupLocation.CenterOwner)
            do window.Show()
            host,window
        | null -> failwith $"ApplicationLifetime is null"
        | alt -> failwith $"Unexpected ApplicationLifetime: {alt.GetType().FullName}"

fsi.GetType().GetProperties()

open FSharp.Compiler.Interactive

typeof<IEventLoop>.Assembly
