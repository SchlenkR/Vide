module Tests

open Fable
open Browser
open Browser.Types
open Fable.Mocha
open Vide

[<AutoOpen>]
module TA =
    let showTest testView = 
        VideApp.Fable.createAndStart (document.getElementById("testHost")) testView
        |> ignore
    let elem id = document.getElementById id |> fun b -> b
    let button id = elem id :?> HTMLButtonElement


let browserTests =
    testList "Getting Started" [
        test "Counter counts" {
            showTest UseCases.GettingStarted.counter
            Expect.equal (elem("result").innerText) "0" "Initial result unexpected"
            button("inc").click()
            Expect.equal (elem("result").innerText) "1" "After 1 inc click"
        }
    ]

let allTests = testList "All Tests" [ browserTests ]

Mocha.runTests allTests |> ignore


////let arithmeticTests =
////    testList "Arithmetic tests" [
////        test "plus works" {
////            Expect.equal (1 + 1) 2 "plus"
////        }
////        test "Test for falsehood" {
////            Expect.isFalse (1 = 2) "false"
////        }
////        testAsync "Test async code" {
////            let! x = async { return 21 }
////            let answer = x * 2
////            Expect.equal 42 answer "async"
////        }
////    ]
