
let makeCounter (initial: int) increment =
    let mutable current = initial
    fun () ->
        do current <- current + increment
        current

let count = makeCounter 0 1
count()

let count2 = makeCounter 100 10
count2()


let makeAdd2Counter () =
    let counter1 = makeCounter 0 1
    let counter2 = makeCounter 100 10
    fun () ->
        let c1 = counter1()
        let c2 = counter2()
        c1 + c2
    
let special = makeAdd2Counter()

special()



let makeAdd2CounterLikeItShouldBe () =
    fun () ->
        let c1 = makeCounter 0 1
        let c2 = makeCounter 100 10
        c1 + c2
  
let special2 = makeAdd2CounterLikeItShouldBe()
special2()