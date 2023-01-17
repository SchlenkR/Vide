open System
open System.IO

open MdnScrape.Helper

let args = Environment.GetCommandLineArgs()

if
    Environment.GetCommandLineArgs() |> Array.contains "--clean" 
    && Directory.Exists(MdnScrape.cacheDir) 
then
    Directory.Delete(MdnScrape.cacheDir, true)

let gen generator relOutFileName =
    let output = 
        let res = MdnScrape.generate ()
        generator res
    let outDir = __SOURCE_DIRECTORY__ </> relOutFileName
    do 
        File.WriteAllText(outDir, output)
        printfn $"Output written to: {outDir}"

let genHtmlApi () =
    "../../src/Vide.Fable/htmlApi.fs"
    |> gen (fun res -> HtmlApiGenerator.generate res.elements res.globalAttrs)

let genHtmlSpec () =
    "../../src/Vide.Fable/htmlSpec.csv"
    |> gen (fun res -> HtmlSpecGenerator.generate res.elements res.globalAttrs)

genHtmlSpec()
