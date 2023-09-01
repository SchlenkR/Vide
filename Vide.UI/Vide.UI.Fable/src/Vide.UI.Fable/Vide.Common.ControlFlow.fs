(*
    This file is intended to be included in Vide projects via link.
    The reason for this is that `vide` CE builder instance can only be defined
    in the specific implementation projects (e.g. Vide.UI.Fable), and builders
    are a thing which is hard to abstract.

    Important:
    In Vide-Fable, this fiele is a copied due to Fable source 
    includes / compilation, it's not possible to lin that file.
*)

[<AutoOpen>]
module Vide.ControlFlow

module ControlFlowBuilderBricks =
    let yieldBranch2 (x: Branch2<Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {
            match x with B1Of2 v -> v | _ -> elsePreserve
            match x with B2Of2 v -> v | _ -> elsePreserve
        }
        
    let inline yieldBranch3 (x: Branch3<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {
            match x with B1Of3 v -> yield v | _ -> yield elsePreserve
            match x with B2Of3 v -> yield v | _ -> yield elsePreserve
            match x with B3Of3 v -> yield v | _ -> yield elsePreserve
        }
        
    let yieldBranch4 (x: Branch4<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {
            match x with B1Of4 v -> v | _ -> elsePreserve
            match x with B2Of4 v -> v | _ -> elsePreserve
            match x with B3Of4 v -> v | _ -> elsePreserve
            match x with B4Of4 v -> v | _ -> elsePreserve
        }


// -------------------------------------------------------------------
// "Yielsd"s
// -------------------------------------------------------------------
    
type VideBaseBuilder with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch4(v)
