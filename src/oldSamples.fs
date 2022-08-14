
let h =
    vide {
        htmlElem 1 {
            htmlElem "1_1"
            
            let! forCount = mut 0
            do forCount.Value <- forCount.Value + 1
            printfn $"----------- forCount = {forCount.Value}"

            htmlElem forCount.Value
        }

        // htmlElem "2"
        // htmlElem 3
        // for x in 0..10 do
        //     htmlElem "4"
        //     htmlElem (5 + x)
        //     // Zero
    }

let evalH = h |> toStateMachine None dummyCtx
evalH()



let d =
    vide {
        htmlElem 1
        htmlElem "2"
        htmlElem 3
        for x in 0..10 do
            htmlElem "4"
            htmlElem (5 + x)
            // Zero
    }

let evalD = d |> toStateMachine None dummyCtx
evalD()


let e =
    vide {
        let! runCount = mut 0
        printfn $"RunCount = {runCount.contents}"
        runCount.contents <- runCount.contents + 1

        htmlElem 1
        htmlElem "2"
        htmlElem 3
        
        for x in 0..10 do
            let! forCount = mut 0
            printfn $"ForCount = {forCount.contents}"
            forCount.contents <- forCount.contents + 1
            
            htmlElem "4"

            if x % 2 = 0 then
                htmlElem $"      IF -  {5 + x}"
    }


let evalE = e |> toStateMachine None dummyCtx
evalE()




let myComponent =
    vide {
        htmlElem "a"
    }

let componentUser =
    vide {
        myComponent
    }

// let (Gen x) = d() in x None


// let a() =
//     test {
//         let! a = { value = 100; state = "a" }
//         let! b = { value = 200; state = 44.2 }
//         { value = a + b; state = 20UL }
        
//         let! c = { value = 33; state = "c" }
//         let! d = { value = 66; state = 44.1 }
//         { value = c + d; state = 10.0 }

//         { value = -77; state = 20.0 }
//         for i in 0..3 do
//             { value = -77; state = 20.0 }

//         let! e = { value = -2; state = [909090] }
//         let! f = { value = -3; state = (0.1, 0.2, 0.3) }
//         for i in 0..3 do
//             { value = e + f + i; state = ("Hello", "World") }
//             { value = e + f + i; state = ("Hello", "World") }
//             { value = e + f + i; state = ("Hello", "World") }

//         { value = e + f; state = ("Hello", "World") }
//     }


// let b() =
//     test {
//         let! a = { value = 100; state = "a" }
//         let! b = { value = 200; state = 44.2 }
//         { value = a + b; state = 20UL }
        
//         let! c = { value = 33; state = "c" }
//         let! d = { value = 66; state = 44.1 }
//         { value = c + d; state = 10.0 }
//         { value = -77; state = 20.0 }
//     }


// let c() =
//     test {
//         { value = 33; state = 20UL }
//         { value = -77; state = 20.0 }
//     }


// let e() =
//     test {
//         if true then
//             { value = -77; state = 20.0 }
//     }
