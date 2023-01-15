module CoreLibAddons

open Vide
open type Html

type textbox() as this =
    inherit Vide.HtmlElementBuilders.input()
    do
        this.ty