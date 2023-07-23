
type LocSta<'v,'s> = LocSta of ('s option -> 'v * 's)

let bind 
    (m: LocSta<'v1,'s1>)
    (f: 'v1 -> LocSta<'v2,'s2>)
    : LocSta<'v2, 's1 * 's2>
    =
    LocSta <| fun s ->
        let ms,fs =
            match s with
            | None -> None,None
            | Some (ms,fs) -> Some ms, Some fs
        let (LocSta m) = m
        let mv,ms = m ms
        let (LocSta f) = f mv
        let fv,fs = f fs
        fv, (ms,fs)

let ret v
    : LocSta<'v,unit>
    =
    LocSta <| fun s -> v,()


type LocStaBuilder() =
    member _.Bind(m, f) = bind m f
    member _.Return(v) = ret v

let locSta = LocStaBuilder()
