module UseCases.Async

open Vide
open type Vide.Html

// TODO: demo for async + clear

let asyncHelloWorld =

    vide {
        p { "loading 1st number ..." }
        let! res1 = async {
            do! Async.Sleep 2000
            return 42
        }
        p { $"1st number is: {res1}" }
        hr

        p { "loading 2nd number..." }
        let! res2 = async {
            do! Async.Sleep 2000
            return 187
        }
        p { $"2nd number is: {res2}" }
        hr
        
        p { "waiting again for a whie..." }
        do! Async.Sleep 2000
        p { "Done :)" }
    }

let asyncInsideHtmlElements =

    // You can also put asyncs inside of HTML elements :)

    vide {
        p { 
            "loading 1st number ..."
            let! res1 = async {
                do! Async.Sleep 2000
                return 42
            }
            $"{res1}"
        }
        hr

        p { 
            "loading 2nd number ..." 
            let! res2 = async {
                do! Async.Sleep 2000
                return 187
            }
            $"{res2}"
        }
        hr
        
        p {
            "waiting (in parallel) for a whie..."
            do! Async.Sleep 2000
            "Done :)"
        }
    }

// TODO: Order of return / yield HTMLElement shouldn't matter

let asyncWithSubsequentResults =

    let myAsyncComponent =
        // TODO: Docu: Before(!) every awaited value must be a return.
        vide {
            p { "loading 1st number ..." }
            return 0

            let! res1 = async {
                do! Async.Sleep 2000
                return 42
            }
            p { $"1st number is: {res1}" }
            hr

            p { "loading 2nd number ..." }
            return res1
                
            let! res2 = async {
                do! Async.Sleep 2000
                return 187
            }
            p { $"2nd number is: {res2}" }
            hr
            
            p { "waiting again for a whie..." }
            return res2
                
            do! Async.Sleep 2000
            p { "Done :)" }

            return 999
        }

    vide {
        let! currRes = Vide.ofMutable 0
        div.class'("async-box") {
            $"Current component result: {currRes.Value}"
        }
        
        let! componentResult = myAsyncComponent
        currRes := componentResult
    }
