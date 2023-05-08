open System

let (s: FormattableString) =
    let x = 10
    $"Hello, {x}"

s
s.ArgumentCount
s.GetArguments()
s.GetArgument(0)
s.Format
