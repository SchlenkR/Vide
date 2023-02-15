module DevApp.Demos

open Microsoft.Maui.Controls
open Vide
open Vide.Maui
open Vide.Maui.GenericApiTest

let noVideDemo () =
    let cp = ContentPage()
    cp.Content <-
        let sl = StackLayout()
        sl.Children.Add(Label(Text = "Label 1"))
        sl.Children.Add(Label(Text = "Label 2"))
        sl
    cp

let simpleVideDemo () =
    vide {
        P<StackLayout>(fun sl ->
            sl.Orientation <- StackOrientation.Horizontal) {
            V<Label>(fun l -> l.Text <- "Label 1")
            V<Label>(fun l -> l.Text <- "Label 2")
            V<Label>(fun l -> l.Text <- "Label 3")
            V<Button>(fun b -> b.Text <- "Label 3")
        }
    }

let start demo = 
    let host = ContentView()
    let app =
        VideApp.createMaui
            host 
            demo 
            (fun app v s -> printfn $"Evaluated {app.EvaluationManager.EvaluationCount} times.")
    do app.EvaluationManager.RequestEvaluation()
    host
