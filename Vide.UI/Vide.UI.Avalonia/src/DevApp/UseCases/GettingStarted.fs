﻿module UseCases.GettingStarted

open Avalonia
open Avalonia.Layout
open Avalonia.Media
open Vide
open Vide.UI.Avalonia

open type Vide.UI.Avalonia.Controls

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        let! count = ofMutable {0}

        VStack
            .HorizontalAlignment(HorizontalAlignment.Center) 
            .VerticalAlignment(VerticalAlignment.Center) {

            TextBlock.Text($"Count = {count.Value}")
            
            HStack {
                Button.Click(fun _ -> count.Value - 1 |> count.Set) { "dec" }
                Button.Click(fun _ -> count.Value + 1 |> count.Set) { "inc" }
            }
        }
    }

//let conditionalAttributes =
//    vide {
//        let! count = ofMutable {0}

//        button.id("hitMe").onclick(fun _ -> count += 1) {
//            $"Hit me! Count = {count.Value}"
//        }
//        div.class'("the-message") {
//            span.id("result").hidden(count.Value <> 5) {
//                "You have the right to defend yourself!"
//            }
//        }
//    }
