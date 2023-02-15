module Vide.WpfishModel

open System
open Vide

//type IWpfishDocument<'n> =
//    inherit INodeDocument<'n>

//[<AbstractClass>] 
//type WpfishContext<'n 
//        when 'n: equality
//        and 'n : (new: unit -> 'n)
//    >
//    (
//        parent: 'n, 
//        evaluationManager: IEvaluationManager,
//        document: IWpfishDocument<'n>
//    ) =
//    inherit NodeContext<'n>(parent, evaluationManager, document)
//    member this.AddElement() =
//        let n = new 'n()
//        do this.KeepChild n
//        do document.AppendChild(parent, n)
//        n

module BuilderHelper =
    let inline checkNode(expectedNodeTypeName: string, actualNodeTypeName: string) =
        match actualNodeTypeName.Equals(expectedNodeTypeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false -> DiscardAndCreateNew
