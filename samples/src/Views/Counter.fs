module Counter

open Vide
open type Html

let view = vide {
    let! count = Mutable.ofValue 0

    div { $"Count = {count.Value}" }
    button.onclick(fun _ -> count -= 1) { "dec" }
    button.onclick(fun _ -> count += 1) { "inc" }
}
