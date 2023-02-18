module Vide.WebModel

open Vide

module BuilderHelper =
    let checkNode (expectedNodeName: string) (actualNodeName: string) =
        // WebSharper has no Equals(.., StrComp) available, so we use this
        // which is enough for HTML element tags.
        match actualNodeName.ToUpper() = expectedNodeName.ToUpper() with
        | true -> Keep
        | false ->
            // TODO: if/else detection? Expected node name: {expectedNodeName}, but was: {actualNodeName}"
            DiscardAndCreateNew
