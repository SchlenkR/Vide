
(*
    This file is intended to be included in Vide projects via link.
    The reason for this is that `vide` CE builder instance can only be defined
    in the specific implementation projects (e.g. Vide.UI.Fable), and builders
    are a thing which is hard to abstract.

    Important:
    In Vide-Fable, this fiele is a copied due to Fable source 
    includes / compilation, it's not possible to link that file.
*)

[<AutoOpen>]
module Vide.ControlFlow


type Branch2<'b1, 'b2> = 
    | B1Of2 of 'b1
    | B2Of2 of 'b2

type Branch3<'b1, 'b2, 'b3> = 
    | B1Of3 of 'b1
    | B2Of3 of 'b2
    | B3Of3 of 'b3

type Branch4<'b1, 'b2, 'b3, 'b4> = 
    | B1Of4 of 'b1
    | B2Of4 of 'b2
    | B3Of4 of 'b3
    | B4Of4 of 'b4

type Branch5<'b1, 'b2, 'b3, 'b4, 'b5> = 
    | B1Of5 of 'b1
    | B2Of5 of 'b2
    | B3Of5 of 'b3
    | B4Of5 of 'b4
    | B5Of5 of 'b5

type Branch6<'b1, 'b2, 'b3, 'b4, 'b5, 'b6> = 
    | B1Of6 of 'b1
    | B2Of6 of 'b2
    | B3Of6 of 'b3
    | B4Of6 of 'b4
    | B5Of6 of 'b5
    | B6Of6 of 'b6

type Branch7<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7> = 
    | B1Of7 of 'b1
    | B2Of7 of 'b2
    | B3Of7 of 'b3
    | B4Of7 of 'b4
    | B5Of7 of 'b5
    | B6Of7 of 'b6
    | B7Of7 of 'b7

type Branch8<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8> = 
    | B1Of8 of 'b1
    | B2Of8 of 'b2
    | B3Of8 of 'b3
    | B4Of8 of 'b4
    | B5Of8 of 'b5
    | B6Of8 of 'b6
    | B7Of8 of 'b7
    | B8Of8 of 'b8

type Branch9<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9> = 
    | B1Of9 of 'b1
    | B2Of9 of 'b2
    | B3Of9 of 'b3
    | B4Of9 of 'b4
    | B5Of9 of 'b5
    | B6Of9 of 'b6
    | B7Of9 of 'b7
    | B8Of9 of 'b8
    | B9Of9 of 'b9

type Branch10<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10> = 
    | B1Of10 of 'b1
    | B2Of10 of 'b2
    | B3Of10 of 'b3
    | B4Of10 of 'b4
    | B5Of10 of 'b5
    | B6Of10 of 'b6
    | B7Of10 of 'b7
    | B8Of10 of 'b8
    | B9Of10 of 'b9
    | B10Of10 of 'b10

type Branch11<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11> = 
    | B1Of11 of 'b1
    | B2Of11 of 'b2
    | B3Of11 of 'b3
    | B4Of11 of 'b4
    | B5Of11 of 'b5
    | B6Of11 of 'b6
    | B7Of11 of 'b7
    | B8Of11 of 'b8
    | B9Of11 of 'b9
    | B10Of11 of 'b10
    | B11Of11 of 'b11

type Branch12<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11, 'b12> = 
    | B1Of12 of 'b1
    | B2Of12 of 'b2
    | B3Of12 of 'b3
    | B4Of12 of 'b4
    | B5Of12 of 'b5
    | B6Of12 of 'b6
    | B7Of12 of 'b7
    | B8Of12 of 'b8
    | B9Of12 of 'b9
    | B10Of12 of 'b10
    | B11Of12 of 'b11
    | B12Of12 of 'b12

type Branch13<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11, 'b12, 'b13> = 
    | B1Of13 of 'b1
    | B2Of13 of 'b2
    | B3Of13 of 'b3
    | B4Of13 of 'b4
    | B5Of13 of 'b5
    | B6Of13 of 'b6
    | B7Of13 of 'b7
    | B8Of13 of 'b8
    | B9Of13 of 'b9
    | B10Of13 of 'b10
    | B11Of13 of 'b11
    | B12Of13 of 'b12
    | B13Of13 of 'b13

type Branch14<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11, 'b12, 'b13, 'b14> = 
    | B1Of14 of 'b1
    | B2Of14 of 'b2
    | B3Of14 of 'b3
    | B4Of14 of 'b4
    | B5Of14 of 'b5
    | B6Of14 of 'b6
    | B7Of14 of 'b7
    | B8Of14 of 'b8
    | B9Of14 of 'b9
    | B10Of14 of 'b10
    | B11Of14 of 'b11
    | B12Of14 of 'b12
    | B13Of14 of 'b13
    | B14Of14 of 'b14

type Branch15<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11, 'b12, 'b13, 'b14, 'b15> = 
    | B1Of15 of 'b1
    | B2Of15 of 'b2
    | B3Of15 of 'b3
    | B4Of15 of 'b4
    | B5Of15 of 'b5
    | B6Of15 of 'b6
    | B7Of15 of 'b7
    | B8Of15 of 'b8
    | B9Of15 of 'b9
    | B10Of15 of 'b10
    | B11Of15 of 'b11
    | B12Of15 of 'b12
    | B13Of15 of 'b13
    | B14Of15 of 'b14
    | B15Of15 of 'b15

type Branch16<'b1, 'b2, 'b3, 'b4, 'b5, 'b6, 'b7, 'b8, 'b9, 'b10, 'b11, 'b12, 'b13, 'b14, 'b15, 'b16> = 
    | B1Of16 of 'b1
    | B2Of16 of 'b2
    | B3Of16 of 'b3
    | B4Of16 of 'b4
    | B5Of16 of 'b5
    | B6Of16 of 'b6
    | B7Of16 of 'b7
    | B8Of16 of 'b8
    | B9Of16 of 'b9
    | B10Of16 of 'b10
    | B11Of16 of 'b11
    | B12Of16 of 'b12
    | B13Of16 of 'b13
    | B14Of16 of 'b14
    | B15Of16 of 'b15
    | B16Of16 of 'b16



module ControlFlowBuilderBricks =

    let inline yieldBranch2 (x: Branch2<Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of2 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of2 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch3 (x: Branch3<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of3 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of3 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of3 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch4 (x: Branch4<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of4 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of4 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of4 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of4 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch5 (x: Branch5<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of5 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of5 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of5 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of5 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of5 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch6 (x: Branch6<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of6 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of6 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of6 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of6 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of6 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of6 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch7 (x: Branch7<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of7 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of7 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch8 (x: Branch8<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of8 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of8 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch9 (x: Branch9<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of9 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of9 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch10 (x: Branch10<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of10 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of10 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch11 (x: Branch11<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of11 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of11 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch12 (x: Branch12<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of12 v -> yield v | _ -> yield elsePreserve   
            match x with B12Of12 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch13 (x: Branch13<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B12Of13 v -> yield v | _ -> yield elsePreserve   
            match x with B13Of13 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch14 (x: Branch14<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B12Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B13Of14 v -> yield v | _ -> yield elsePreserve   
            match x with B14Of14 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch15 (x: Branch15<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B12Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B13Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B14Of15 v -> yield v | _ -> yield elsePreserve   
            match x with B15Of15 v -> yield v | _ -> yield elsePreserve   
        }   

    let inline yieldBranch16 (x: Branch16<Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>, Vide<unit,_,_>>) =
        vide {     
            match x with B1Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B2Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B3Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B4Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B5Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B6Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B7Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B8Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B9Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B10Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B11Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B12Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B13Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B14Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B15Of16 v -> yield v | _ -> yield elsePreserve   
            match x with B16Of16 v -> yield v | _ -> yield elsePreserve   
        }   


// -------------------------------------------------------------------
// "Yielsd"s
// -------------------------------------------------------------------
    
type VideBaseBuilder with     
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch2(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch3(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch4(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch5(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch6(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch7(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch8(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch9(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch10(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch11(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch12(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch13(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch14(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch15(v)   
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch16(v)   

    