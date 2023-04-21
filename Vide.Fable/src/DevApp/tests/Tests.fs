module Tests

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

Mocha.runTests arithmeticTests |> ignore
