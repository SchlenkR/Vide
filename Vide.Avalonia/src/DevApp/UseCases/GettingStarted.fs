module UseCases.GettingStarted

open Vide
open Avalonia.Layout
open type Vide.AvaloniaControls

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        let! count = Vide.ofMutable 0

        VStack
            .HorizontalAlignment(HorizontalAlignment.Center) 
            .VerticalAlignment(VerticalAlignment.Center) {

            TextBlock.Text($"Count = {count.Value}")
            
            HStack {
                Button.Click(fun _ -> count -= 1) { "dec" }
                Button.Click(fun _ -> count += 1) { "inc" }
            }
        }
    }

//let conditionalAttributes =
//    vide {
//        let! count = Vide.ofMutable 0

//        button.id("hitMe").onclick(fun _ -> count += 1) {
//            $"Hit me! Count = {count.Value}"
//        }
//        div.class'("the-message") {
//            span.id("result").hidden(count.Value <> 5) {
//                "You have the right to defend yourself!"
//            }
//        }
//    }
