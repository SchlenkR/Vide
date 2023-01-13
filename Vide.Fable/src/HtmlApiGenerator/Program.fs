open System
open System.IO

open MdnScrape.Helper

let args = Environment.GetCommandLineArgs()

if
    Environment.GetCommandLineArgs() |> Array.contains "--clean" 
    && Directory.Exists(MdnScrape.cacheDir) 
then
    Directory.Delete(MdnScrape.cacheDir, true)

let output = 
    let res = MdnScrape.generate ()
    HtmlApiGenerator.generate res.elements res.globalAttrs
let outDir = __SOURCE_DIRECTORY__ </> "../../Vide.Fable/lib/htmlApi.fs"
do 
    File.WriteAllText(outDir, output)
    printfn $"Output written to: {outDir}"
