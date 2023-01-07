open System
open System.Threading.Tasks

let res = task {
    do! Task.Delay 1000
    return 10
}

res
