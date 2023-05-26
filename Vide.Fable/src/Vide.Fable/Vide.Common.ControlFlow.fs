(*
    This file is intended to be included in Vide projects via link.
    The reason for this is that `vide` CE builder instance can only be defined
    in the specific implementation projects (e.g. Vide.Fable), and builders
    are a thing which is hard to abstract.

    Important:
    In Vide-Fable, this fiele is a copied due to Fable source 
    includes / compilation, it's not possible to lin that file.
*)

[<AutoOpen>]
module Vide.ControlFlow

module ControlFlowBuilderBricks =
    let yieldBranch2 x =
        vide {
            match x with B1Of2 v -> ensureVide v | _ -> elsePreserve
            match x with B2Of2 v -> ensureVide v | _ -> elsePreserve
        }
        
    let yieldBranch3 x =
        vide {
            match x with B1Of3 v -> ensureVide v | _ -> elsePreserve
            match x with B2Of3 v -> ensureVide v | _ -> elsePreserve
            match x with B3Of3 v -> ensureVide v | _ -> elsePreserve
        }
        
    let yieldBranch4 x =
        vide {
            match x with B1Of4 v -> ensureVide v | _ -> elsePreserve
            match x with B2Of4 v -> ensureVide v | _ -> elsePreserve
            match x with B3Of4 v -> ensureVide v | _ -> elsePreserve
            match x with B4Of4 v -> ensureVide v | _ -> elsePreserve
        }


// -------------------------------------------------------------------
// "Yielsd"s 
// -------------------------------------------------------------------
    
type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
    
type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
        
type RenderValC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
    
type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
    
type RenderValCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)
