
#r "nuget: Vide.Common.DSP"

open Vide
open Vide.DSP

let zipped =
    dsp {
        let! a = Vide.ofSeq ["a"; "b"; "c"]
        let! b = Vide.ofSeq ["1"; "2"; "3"]
        let! c = Vide.ofSeq ["+"; "-"; "*"]
        return $"{a}{b}{c}"
    }
    |> Vide.DSP.Gen.Eval.toSeq id

zipped |> Seq.take 10 |> Seq.toList

