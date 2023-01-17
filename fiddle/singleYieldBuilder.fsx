
module MaxOneYield =
    // --------------------------------------------
    // Recipe for a builder that limits
    // the number of yields to a maximum of one.
    // --------------------------------------------

    // We need a distinct type for a "Zero" element.
    type ZeroChild = ZeroChild

    type MaxOneChildBuilder() =
        member _.Yield(value: 'a) = value
    
        member _.Zero() = ZeroChild
    
        // `builder { yield value }` results in a combination
        // of the builder's Zero element and the yielded value.
        // Let's discard the zero element and emit the value.
        member _.Combine(a: ZeroChild, b: 'a) = b
    
        // Unwrap the delayed func immediately and emit the result
        // "f" in that case is the value returned by Yield and Combine.
        member _.Delay (f: unit -> 'a) = f ()
        member _.Run (value: 'a) = value

    let maxOne = MaxOneChildBuilder()


    // ----------
    // Some tests
    // ----------

    // compiles
    let one = 
        maxOne {
            1
        }

    // does not compile
    let oneAndTwo = 
        maxOne {
            1
            2
        }


module ExactlyOneYield =
    // --------------------------------------------
    // Recipe for a builder that requires
    // exactly one yield.
    // --------------------------------------------

    // We need a distinct type for a "Zero" element:
    type ZeroChild = ZeroChild
    
    // ...and a n indicator for a yielded value:
    type SingleChild<'a> = SingleChild of 'a

    type ExactlyOneChildBuilder() =
        //In combine, we need 2 disjunct types for "Zero" and
        // "a yielded value", so we wrap the yielded value
        // with SingleChild:
        member _.Yield(value: 'a) = SingleChild value
    
        member _.Zero() = ZeroChild
    
        // `builder { yield value }` results in a combination
        // of the builder's Zero element and the yielded value.
        // Let's discard the zero element and emit the value.
        member _.Combine(a: ZeroChild, b: SingleChild<'a>) = b
    
        // Unwrap the delayed func immediately and emit the result
        // (which is still wrapped in SingleChild).
        // "f" in that case is the value returned by Yield and Combine.
        // We finally unwrap the "SingleChild" in the "Run" method to
        // emit the yielded value:
        member _.Delay (f: unit -> SingleChild<'a>) = f ()
        member _.Run (SingleChild value: SingleChild<'a>) = value

    let exactlyOne = ExactlyOneChildBuilder()


    // ----------
    // Some tests
    // ----------

    // compiles
    let one = 
        exactlyOne {
            1
        }

    // does not compile
    let oneAndTwo = 
        exactlyOne {
            ()
        }

    // does not compile
    let oneAndTwo = 
        exactlyOne {
            1
            2
        }


