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
    let elem id = document.getElementById id
    let textOf id = (document.getElementById id).innerText
    let button id = elem id :?> HTMLButtonElement


let gettingStartedTests =
    testList "Getting Started" [
        test "Counter counts" {
            showTest UseCases.GettingStarted.counter
            let inc = button("inc")
            let dec = button("dec")

            Expect.equal (textOf "result") "0" "Initial result unexpected"
            
            inc.click()
            Expect.equal (textOf "result") "1" "After 1 inc click"
            
            inc.click()
            inc.click()
            Expect.equal (textOf "result") "3" "After another 2 inc clicks"
            
            dec.click()
            dec.click()
            dec.click()
            Expect.equal (textOf "result") "0" "After 3 dec clicks"
        }
        
        test "Conditional attributes" {
            showTest UseCases.GettingStarted.conditionalAttributes
            let btn = button("hitMe")
            let result = elem("result")
            let assertIsHidden clicks = Expect.isTrue (result.hasAttribute("hidden")) $"Result should be 'hidden' after {clicks} clicks."
            let assertIsNotHidden clicks = Expect.isFalse (result.hasAttribute("hidden")) $"Result should not be 'hidden' after {clicks} clicks."
            
            btn.click()
            assertIsHidden 1

            btn.click()
            assertIsHidden 2
            
            btn.click()
            assertIsHidden 3
            
            btn.click()
            assertIsHidden 4
            
            btn.click()
            assertIsNotHidden 5
            
            btn.click()
            assertIsHidden 6
        }
    ]

let allTests = 
    [ 
        gettingStartedTests  
    ]
    |> testList "All Tests"

Mocha.runTests allTests |> ignore
