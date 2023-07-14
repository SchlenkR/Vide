
// ---------------------------------------------------
// This is a simplified demo of the core concept of
// Vide (aka LocSta, aka 1-step-seq, etc.)
// ---------------------------------------------------

module LocSta =
    type LocSta<'v,'s> = LocSta of ('s option -> 'v * 's)

    let bind 
        (f: 'v1 -> LocSta<'v2,'s2>)
        (m: LocSta<'v1,'s1>)
        : LocSta<'v2, 's1 * 's2>
        =
        LocSta <| fun s ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let (LocSta m) = m
            let mv,ms = m ms
            let (LocSta f) = f mv
            let fv,fs = f fs
            fv, (ms,fs)

    let ret v
        : LocSta<'v,unit>
        =
        LocSta <| fun s -> v,()

    type LocStaBuilder() =
        member _.Bind(m, f) = bind f m
        member _.Return(v) = ret v

    let locSta = LocStaBuilder()

    
module HelperAndBuildingBlocks =
    open LocSta
    
    module Seq =
        let ofLocSta (LocSta l) =
            seq {
                let mutable currState = None
                while true do
                    let v,s = l currState
                    do currState <- Some s
                    yield v
            }

    let inline count exclStart increment =
        LocSta <| fun s ->
            let s = s |> Option.defaultValue exclStart
            let res = s + increment
            res,res

// -------------------
// Test
// -------------------

open LocSta
open HelperAndBuildingBlocks
    
let x = locSta {
    // step-by step evaluation of each counter
    let! c1 = count 0.0 0.1
    let! c2 = count 10.0 1.0
    return c1 + c2
}

// yields: [11.1; 12.2; 13.3; 14.4; 15.5; 16.6; 17.7; 18.8; 19.9; 21.0]
let res = x |> Seq.ofLocSta |> Seq.take 10 |> Seq.toList
