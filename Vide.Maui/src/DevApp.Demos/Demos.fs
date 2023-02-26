module DevApp.Demos

open Microsoft.Maui.Controls
open Vide
open Vide.GenericMauiApi

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
            sl.HorizontalOptions <- LayoutOptions.Center
            sl.VerticalOptions <- LayoutOptions.Center) {
                let! count = Vide.ofMutable 0

                P<StackLayout>() {
                    for x in 0..5 do
                        V<Label>(fun l -> l.Text <- $"For loop at index {x}")
                }

                P<StackLayout>(fun sl ->
                    sl.Orientation <- StackOrientation.Horizontal) {
                        V<Label>(fun l -> l.Text <- "Click count: ")
                        V<Label>(fun l -> 
                            l.Text <- count.Value.ToString()
                            l.FontAttributes <- FontAttributes.Bold)
                }
                
                V<Button>(fun b -> 
                    b.Text <- "Click me!"
                    b.Margin <- Microsoft.Maui.Thickness(0, 5))
                    .OnInit(fun x -> x.node.Clicked.Add(fun _ -> count.Value <- count.Value + 1))
            }
    }

let start demo = 
    let host = ContentView()
    let app =
        let onEvaluated (app: VideApp<_,_,_>) v s =
            printfn $"Evaluated {app.EvaluationManager.EvaluationCount} times."
        VideApp.Maui.create host demo onEvaluated
    do app.EvaluationManager.RequestEvaluation()
    host
