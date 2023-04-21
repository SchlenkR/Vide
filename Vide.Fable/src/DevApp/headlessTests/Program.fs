[<EntryPoint>]
let main argv =
    "../dist"
    |> System.IO.Path.GetFullPath
    |> Puppeteer.runTests
    |> Async.RunSynchronously
