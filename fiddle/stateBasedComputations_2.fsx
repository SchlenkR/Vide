
type Block<'output, 'state> = option<'state> -> ('output * 'state)

type BlockBuilder() =
    member this.Bind(m: Block<'o1, 's1>, f: 'o1 -> Block<'o2, 's2>) : Block<_,_> =
        fun stateOption ->
            // wie kommen wir von option<('s1 * 's2)> nach (option<'s1> * option<'s2>) ?
            let mPrevState,fPrevState =
                match stateOption with
                | None -> None,None
                | Some (s1, s2) -> Some s1, Some s2
            
            let (mout,mNewState) = m mPrevState

            let fBlock = f mout
            let (fout,fNewState) = fBlock fPrevState

            let overallNewState = mNewState,fNewState
            fout,overallNewState
    member this.Return(x: 'a) : Block<_,_> = fun _ -> x,()
let block = BlockBuilder()

let counter (initial: int) increment : Block<int, 'state> =
    fun maybeState ->
        let current = maybeState |> Option.defaultValue initial
        let value = current + increment
        value,value

let makeAdd2CounterLikeItShouldBe =
    block {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        let! delayed = delay
        return float (c1 + c2)
    }



let o1,s1 = makeAdd2CounterLikeItShouldBe None
let o2,s2 = makeAdd2CounterLikeItShouldBe (Some s1)


let blockToIEnumerable (block: Block<'o,'s>) =
    let mutable currentState = None
    seq {
        while true do
            let value,newState = block currentState
            do currentState <- Some newState
            yield value
    }

let myStream =
    block {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        return float (c1 + c2)
    }
    |> blockToIEnumerable

myStream |> Seq.take 5 


//let makeAdd2CounterLikeItShouldBe : Block<_,int> =
//    (makeCounter 0 1) |> bind (fun c1 ->
//    (makeCounter 100 10) |> bind (fun c2 ->
//    retBlock (c1 + c2)
//    )
//    )


//let makeAdd2CounterLikeItShouldBe : Block<_,int> =
//    let! (c1: int) = (makeCounter 0 1) : Block<int,int>
//    let! (c2: int) = (makeCounter 100 10) : Block<int,int>
//    return c1 + c2
  
