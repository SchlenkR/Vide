module Vide.Fable.Samples.App

open Browser
open Vide
open type Html

let sampleContent = vide {
    let! count = Mutable.ofValue 0

    div { $"Count = {count.Value}" }
    button.onclick(fun _ -> count -= 1) { "dec" }
    button.onclick(fun _ -> count += 1) { "inc" }
}

let host = document.getElementById("sample")
let app = App.createFableWithObjState host sampleContent (fun _ _ -> ())
console.log("Hurz")
do app.RequestEvaluation()
