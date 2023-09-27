module UseCases.GettingStarted

open Avalonia
open Avalonia.Layout
open Avalonia.Media
open Vide
open Vide.UI.Avalonia

open type Vide.UI.Avalonia.Controls

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

open System
open System.Runtime.CompilerServices
open Avalonia.Interactivity

[<Extension>]
type Extensions =
    [<Extension>]
    static member inline Click2<'nb,^e,'n,'c 
            when 'nb :> NodeBuilder<^e,'c> 
            and ^e :> Avalonia.Controls.Control
            and ^e : (member Click: IEvent<EventHandler<RoutedEventArgs>,RoutedEventArgs>)
        > 
        (this: 'nb, handler) 
        =
        this.onInit(fun x ->
            // let wrappedHandler = Event.handle x.node x.host handler
            // let click = x.node.Click
            // let event = x.node.Click
            // Event.add wrappedHandler x.node.Click.Publish
            Event.add handler x.node.Click
        )

let btn = Avalonia.Controls.Button()
let click = btn.Click
        
let counter =
    vide {
        let! count = Vide.ofMutable 0

        VStack
            .HorizontalAlignment(HorizontalAlignment.Center) 
            .VerticalAlignment(VerticalAlignment.Center) {

            TextBlock.Text($"Count = {count.Value}")
            
            HStack {
                Button.Click2(fun _ -> count.Value - 1 |> count.Set) { "dec" }
                Button.Click2(fun _ -> count.Value + 1 |> count.Set) { "inc" }
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
