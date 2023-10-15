
// #r "nuget: Vide.UI.Avalonia.Interactive"
#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"

// #r "nuget: Vide.UI.Avalonia"
// #load "../Vide.UI.Avalonia.Interactive/Interactive.fs"
#I @"..\Vide.UI.Avalonia.Interactive\bin\Debug\net7.0"
#r "Vide.Common.dll"
#r "Vide.Common.UI.dll"
#r "Vide.UI.Avalonia.dll"
#r "Vide.UI.Avalonia.Interactive.dll"

Vide.UI.Avalonia.Interactive.guardInit ()

// ^ -------------------------------------------------------------
// |_ This is the boilerplate to make the sample work in fsi.
//    Evaluate this _once and separate_ from the rest of the sample.
// ---------------------------------------------------------------


// ---------------------------------------------------------------
// Here starts the actual sample ...
// ---------------------------------------------------------------

open System
open Vide
open Vide.UI.Avalonia
open type Vide.UI.Avalonia.Controls
open type Vide.UI.Avalonia.AvaloniaControlsDefaults

let loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies sed, dolor."

let window = Interactive.createWindow 300. 500.


vide {
    Grid.RowDefinitions("auto,*") {
        H1
            .Grid().Row(0)
            .HorizontalAlignment(HA.Center)
            .Text("Hello Avalonia")
        VStack {
            let! showLongText = ofMutable { false }
            
            CheckBox
                .Margin(10)
                .BindIsChecked(showLongText)
                .Content("Show long text")

            match showLongText.Value with
            | true -> B1Of2 <| vide {
                TextBlock
                    .Grid().Row(1)
                    .Margin(10).TextWrapping(Wrap.Wrap)
                    .Text(loremIpsum)
                }
            | false -> B2Of2 <| vide {
                let! count = ofMutable { 0 }
                Button
                    .Margin(10).HorizontalAlignment(HA.Center)
                    .Click(fun _ -> count.Value <- count.Value + 1)
                    { $"Click me {count.Value} times" }
                }
        }
    }
}
|> fun view -> Interactive.showView view window
