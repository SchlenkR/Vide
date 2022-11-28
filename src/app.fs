module App

open Browser.Dom
open Browser.Types
open Fable.Core.JS
open Vide
open Vide.Fable
open type Html


// counter sample
let videApp =
    vide {
        let! count = Mutable.ofValue 0

        div {
            $"Count = {count.Value}"
        }

        button .onclick(fun _ -> count -= 1) { "dec" }
        button .onclick(fun _ -> count += 1) { "inc" }
    }

(*
    Some more samples to uncomment ...
*)

(* hello world sample *)
// let videApp =
//     vide {
//         "Hello World"
//     }

(* conditionalAttributes sample *)
// let videApp =
//     vide {
//         let! count = Mutable.ofValue 0

//         button .onclick(fun _ -> count += 1) {
//             $"Hit me! Count = {count.Value}"
//         }
//         div .class'("the-message") {
//             span .hidden(count.Value <> 5) {
//                 "You have the right to defend yourself!"
//             }
//         }
//     }

(* conditionalIfs sample *)
// let videApp =
//     vide {
//         let! count = Mutable.ofValue 0

//         button .onclick(fun _ -> count += 1) {
//             $"Hit me! Count = {count.Value}"
//         }

//         if count.Value = 5 || count.Value = 6 then
//             let! valueString = preserve "Hello String"
//             div .class'("the-message") { 
//                 $"You have the right to defend yourself! (string value {valueString})" 
//             }
//         if count.Value <> 5 then
//             let! valueInt = preserve 42
//             p { $"not yet ... with int value {valueInt}" }
//     }

(* conditionalIfElse sample *)
// let videApp =
//     vide {
//         let! count = Mutable.ofValue 0

//         button .onclick(fun _ -> count += 1) {
//             $"Hit me! Count = {count.Value}"
//         }

//         // TODO: That should not be used at all? And: That this seems to work
//         // is only an edge case, because state has same type
//         if count.Value = 5 then
//             div .class'("the-message") { 
//                 $"You have the right to defend yourself!" 
//             }
//         else
//             p { $"not yet ..." }
//     }

(* simpleFor sample *)
// let videApp =
//     vide {
//         for x in 0..5 do
//             div .class'("card") { $"I'm element no. {x}" }
//     }

(* statelessFor sample *)
// let videApp =
//     let nextNum() = System.Random().Next(10000)
//     vide {
//         let! items = Mutable.ofValue []
//         let add1 _ = items := items.Value @ [nextNum()]
//         let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
//         let removeAll _ = items :=  []

//         button .onclick(add1) { "Add One" }
//         button .onclick(add100) { "Add 100" }
//         button .onclick(removeAll) { "Remove All" }
        
//         for x in items.Value do
//             div .class'("card") {
//                 let removeMe _ = items := items.Value |> List.except [x]
//                 button .onclick(removeMe) { $"Remove {x}" }
//         }
//     }

(* statefulFor sample *)
// let videApp =
//     let nextNum() = System.Random().Next(10000)
//     vide {
//         let! items = Mutable.ofValue []
//         let add1 _ = items := items.Value @ [nextNum()]
//         let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
//         let removeAll _ = items := []

//         button .onclick(add1) { "Add One" }
//         button .onclick(add100) { "Add 100" }
//         button .onclick(removeAll) { "Remove All" }
        
//         for x in items.Value do
//             div .class'("card") {
//                 let removeMe _ = items := items.Value |> List.except [x]
//                 button .onclick(removeMe) { $"Remove {x}" }

//                 let! count = Mutable.ofValue 0
//                 button .onclick(fun _ -> count -= 1) { "dec" }
//                 $"{count.Value}  "
//                 button .onclick(fun _ -> count += 1) { "inc" }
//         }
//     }

let appHolder = document.getElementById("app")
let videMachine = Vide.Fable.startApp appHolder videApp (fun sender state -> ())
videMachine.Eval()
