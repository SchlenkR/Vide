module Vide.WpfishModel

open System
open Vide

module BuilderHelper =
    let inline checkNode(expectedNodeTypeName: string, actualNodeTypeName: string) =
        match actualNodeTypeName.Equals(expectedNodeTypeName, StringComparison.OrdinalIgnoreCase) with
        | true -> Keep
        | false -> DiscardAndCreateNew
