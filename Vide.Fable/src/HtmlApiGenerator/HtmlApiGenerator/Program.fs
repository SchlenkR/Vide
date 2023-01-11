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
    HtmlApiGenerator.generate res.elements
let outDir = __SOURCE_DIRECTORY__ </> "../../lib/htmlApi2.fs"
do 
    File.WriteAllText(outDir, output)
    printfn $"Output written to: {outDir}"
