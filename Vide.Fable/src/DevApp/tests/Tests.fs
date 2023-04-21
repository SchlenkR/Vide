module Tests

open Fable
open Browser
open Browser.Types
open Fable.Mocha

let arithmeticTests =
    testList "Arithmetic tests" [
        test "plus works" {
            Expect.equal (1 + 1) 2 "plus"
        }

        test "Test for falsehood" {
            Expect.isFalse (1 = 2) "false"
        }

        testAsync "Test async code" {
            let! x = async { return 21 }
            let answer = x * 2
            Expect.equal 42 answer "async"
        }
    ]

let browserTests =
    testList "Browser Tests" [
        test "test 1" {
            let e1 = document.getElementById("dummy") :?> HTMLButtonElement
            Expect.isNotNull e1 "e1 is null"
        }
    ]

let allTests = testList "All" [ arithmeticTests; browserTests ]

Mocha.runTests allTests |> ignore
