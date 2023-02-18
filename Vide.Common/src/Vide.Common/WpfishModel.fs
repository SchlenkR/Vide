module Vide.WpfishModel

open Vide

module BuilderHelper =
    let inline checkNode(expectedNodeTypeName: string, actualNodeTypeName: string) =
        match actualNodeTypeName = expectedNodeTypeName with
        | true -> Keep
        | false -> DiscardAndCreateNew
