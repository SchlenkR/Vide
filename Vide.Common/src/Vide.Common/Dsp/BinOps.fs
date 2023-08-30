namespace Vide

// TODO: That doesn't work when we use InlineIfLambda

// let inline private binOpBoth
//     (left: Vide<'v1,'s1,'c>)
//     (right: Vide<'v2,'s2,'c>)
//     ([<InlineIfLambda>] f)
//     =
//     dsp {
//         let! l = left
//         let! r = right
//         return f l r 
//     }

// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpBoth left right (+)
//     static member inline (-) (left, right) = binOpBoth left right (-)
//     static member inline (*) (left, right) = binOpBoth left right (*)
//     static member inline (/) (left, right) = binOpBoth left right (/)
//     static member inline (%) (left, right) = binOpBoth left right (%)

// let inline private binOpLeft left right f =
//     dsp {
//         let l = left
//         let! r = right
//         return f l r
//     }

// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpLeft left right (+)
//     static member inline (-) (left, right) = binOpLeft left right (-)
//     static member inline (*) (left, right) = binOpLeft left right (*)
//     static member inline (/) (left, right) = binOpLeft left right (/)
//     static member inline (%) (left, right) = binOpLeft left right (%)

// let inline private binOpRight
//     (left: Vide<'v1,'s1,'c>)
//     (right: Vide<'v2,'s2,'c>)
//     ([<InlineIfLambda>] f)
//     =
//     dsp {
//         let! l = left
//         let r = right
//         return f l r
//     }

// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpRight left right (+)
//     static member inline (-) (left, right) = binOpRight left right (-)
//     static member inline (*) (left, right) = binOpRight left right (*)
//     static member inline (/) (left, right) = binOpRight left right (/)
