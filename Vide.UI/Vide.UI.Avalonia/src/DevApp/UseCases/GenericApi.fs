module UseCases.GenericApi

open Vide
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia
open Vide.GenericAvaloniaApi

let simpleVideDemo =
    vide {
        P<StackPanel>(fun sl ->
            sl.HorizontalAlignment <- HorizontalAlignment.Center
            sl.VerticalAlignment <- VerticalAlignment.Center) {
                let! count = ofMutable {5}

                P<StackPanel>() {
                    for x in 0..count.Value do
                        V<TextBlock>(fun l -> l.Text <- $"For loop at index {x}")
                }

                P<StackPanel>(fun sl ->
                    sl.Orientation <- Orientation.Horizontal) {
                        V<TextBlock>(fun l -> l.Text <- "Click count: ")
                        V<TextBlock>(fun l -> 
                            l.Text <- count.Value.ToString()
                            l.FontWeight <- FontWeight.Bold)
                }
                
                P<StackPanel>(fun sl ->
                    sl.Orientation <- Orientation.Horizontal) {
                        V<Button>(fun b -> 
                            b.Content <- "Dec"
                            b.Margin <- Thickness(0, 5))
                            .onInit(fun x -> x.node.Click.Add(fun _ -> count.Value <- count.Value - 1))
                        V<Button>(fun b -> 
                            b.Content <- "Inc"
                            b.Margin <- Thickness(0, 5))
                            .onInit(fun x -> x.node.Click.Add(fun _ -> count.Value <- count.Value + 1))
                }
            }
    }
