open System
open System.IO

open W3schoolScrape

let args = Environment.GetCommandLineArgs()

if
    Environment.GetCommandLineArgs() |> Array.contains "--clean" 
    && Directory.Exists(cacheDir) 
then
    Directory.Delete(cacheDir, true)

let gen generator relOutFileName =
    let output = 
        let res = generate ()
        generator res
    let outDir = __SOURCE_DIRECTORY__ </> relOutFileName
    do 
        File.WriteAllText(outDir, output)
        printfn $"Output written to: {outDir}"

let genHtmlApi () =
    "../Vide.UI.Fable/HtmlApi.fs"
    |> gen (fun res -> HtmlApiGenerator.generate res.elements res.globalAttrs res.globalEvents)

genHtmlApi ()
