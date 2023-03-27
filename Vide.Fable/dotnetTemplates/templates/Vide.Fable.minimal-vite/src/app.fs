module App

open Browser
open Vide

// ---------------------------------------------
// Uncomment the sample you would like to see!
// ---------------------------------------------
let sample =
    //Counter.view
    TodoList.view

// Bootstrap the app :)
do
    let host = document.getElementById("sample")
    let app = App.createFableWithObjState host sample (fun _ _ -> ())
    app.RequestEvaluation()
