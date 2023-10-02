
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let currDir = System.Environment.CurrentDirectory

#r "nuget: Fake.Core.Process"
#r "nuget: Fake.IO.FileSystem"

open System

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators


let nugetServer = "https://api.nuget.org/v3/index.json"
let nugetPushEnvVarName = "nuget_push"
let packPath = 
    let packFolderName = "../.pack"
    Path.combine currDir packFolderName
    
[<AutoOpen>]
module Helper =
    type Shell with
        static member ExecSuccess (cmd: string, ?args: string, ?dir: string) =
            let res = Shell.Exec(cmd, ?args = args, ?dir = dir)
            if res <> 0 then failwith $"Shell execute was not successful: {res}" else ()

    type Args() =
        let singleArg = fsi.CommandLineArgs.[1..] |> Array.tryExactlyOne
        let mutable switches : string list = []
        member this.hasArg arg =
            switches <- arg :: switches
            singleArg |> Option.map (fun a -> a = arg) |> Option.defaultValue false
        member this.assertArgs() =
            match singleArg with
            | None ->
                let switches = switches |> String.concat "|"
                let msg = $"USAGE: dotnet fsi build.fsx [{switches}]"
                printfn "%s" msg
                Environment.Exit -1
            | _ -> ()

let cleanPackFolder () =
    Trace.trace $"Cleaning dir {packPath} ..."
    !! packPath |> Shell.cleanDirs 

let build proj =
    Trace.trace $"Build: {proj}"
    Shell.ExecSuccess ("dotnet", $"build {proj}")

let test proj =
    Trace.trace $"Test: {proj}"
    Shell.ExecSuccess ("dotnet", $"test {proj}")

let pack proj =
    Trace.trace $"Pack: {proj}"
    Shell.ExecSuccess ("dotnet", sprintf "pack %s -o %s -c Release" proj packPath)

//let format = "format", fun () ->
//    // TODO: core.fs
//    Shell.ExecSuccess ("dotnet", $"fantomas .\src\ --recurse")

// TODO: git tag + release
let publish () =
    let nugetApiKey =
        match Environment.environVar nugetPushEnvVarName with
        | null -> failwith $"Environment variable {nugetPushEnvVarName} is not set"
        | v -> v
    for p in !! $"{packPath}/*.nupkg" do
        Trace.trace $"NUGET push {p} ..."
        Shell.ExecSuccess ("dotnet", $"nuget push {p} -k {nugetApiKey} -s {nugetServer} --skip-duplicate")
