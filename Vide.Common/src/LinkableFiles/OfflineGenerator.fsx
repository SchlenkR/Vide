#r "nuget: Trulla"

let numberOfBranches = [2..16]

open System.IO
open Trulla

let [<Literal>] renderTemplate = """
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

{{for b in branches}}
type Branch{{b.n}}<{{for n in b.branchIterations|, }}'b{{n}}{{end}}> = {{for n in b.branchIterations}}
    | B{{n}}Of{{b.n}} of 'b{{n}}{{end}}
{{end}}


module ControlFlowBuilderBricks =
{{for b in branches}}
    let inline yieldBranch{{b.n}} (x: Branch{{b.n}}<{{for n in b.branchIterations|, }}Vide<unit,_,_>{{end}}>) =
        vide {     {{for n in b.branchIterations}}
            match x with B{{n}}Of{{b.n}} v -> yield v | _ -> yield elsePreserve   {{end}}
        }   
{{end}}

// -------------------------------------------------------------------
// "Yielsd"s
// -------------------------------------------------------------------
    
type VideBaseBuilder with     {{for b in branches}}
    member _.Yield(v) = ControlFlowBuilderBricks.yieldBranch{{b.n}}(v)   {{end}}

    """


type Tmpl = Template<renderTemplate>

let templateModel =
    Tmpl.Root(
        [
            for b in numberOfBranches do
                Tmpl.b(
                    [ for x in 1..b do x.ToString()],
                    b.ToString())
        ])

// Render and print it:
let renderedTemplate = Tmpl.Render templateModel

do printfn $"{renderedTemplate}"

do
    let outFile = Path.Combine(__SOURCE_DIRECTORY__, "Vide.Common.ControlFlow.fs")
    if File.Exists(outFile) then
        File.Delete(outFile)
    File.WriteAllText(outFile, renderedTemplate)
