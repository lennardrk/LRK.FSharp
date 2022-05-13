module PairExamples

open LRK.Datastructures

// Creating a Pair
let p = Pair.Unit 1 "2"
let p2 = Pair.Unit "2" 1

// Creating a Pair, but only left
Pair<int, unit>.UnitL 1 |> ignore

// Creating a Pair, but only right (result is Pair<unit, int>)
Pair<int, unit>.UnitR 1 |> ignore

// Creating a Pair, but reversed
Pair<int, int>.UnitRL 1 2 |> ignore

// Left and Right side deconstructors
let a : int = Pair.OutL p
let b : string = Pair.OutR p

// When deconstructing both you need to resolve both into 1 type
let format (x:int) (y: string) = sprintf "%i, %s" x y
let ab = Pair.Deconstruct (format) p

// When creating a mapping function you need two functions to operate both sides, making it a bi-functor and endofunctor
let map1 (x:int) = x * x 
let map2 (x:string) = x + x 
let mapping = Pair.Map(map1)(map2) // with partial application you can create new functions, so you may reuse the mapper later.
let mappedP = mapping p

// Same goes for MapLeft and MapRight
let leftMapper = Pair<int,string>.MapLeft(map1)
let rightMapper = Pair<int,string>.MapRight(map2)
let bothMapper = leftMapper >> rightMapper //would be the same as 'mapping'

// The various joins are a bit awkward. You need to lose information in order to join two Pairs together. +The opposite side of a pair is forgotten
let nestedP : Pair<Pair<int, string>,Pair<int, string>> = Pair.Unit p p
let joinedPLR : Pair<int, string> = Pair.JoinLR nestedP
let joinedPRL : Pair<string, int> = Pair.JoinRL nestedP

// Bind on Pair is also a bit awkward. 
// Bind shines more with types like Option, Either and coroutine, where there is an added benefit. 
let binded : Pair<int, string> = 
    Pair.BindLR(fun x -> Pair<int, unit>.UnitL x)(fun y -> Pair<string, unit>.UnitR y) p

// Bind2 becomes too bothersome to be actually used,
//  but it does provide a proper `foundation` for HoritontalBind2 and DiagonalBind2
let bindedPairs = Pair.Bind2
                    (fun x1 x2 -> x1 + x2)
                    (fun x1 y2 -> sprintf "%i, %s" x1 y2)
                    (fun x1x2 x1y2 -> sprintf "%i, %s" x1x2 x1y2 |> Pair<string, unit>.UnitL)
                    (fun y1 x2 -> sprintf "%s, %i" y1 x2)
                    (fun y1 y2 -> sprintf "%s %s" y1 y2)
                    (fun y1x2 y1y2 -> sprintf "%s %s" y1x2 y1y2 |> Pair<string, unit>.UnitR) p p

// Horzintal Map Pairs x1 with x2 and y1 with y2.
// For example, you could create a function that calculates the euclidian distance from two Pairs.
let ReverseArgs f x y = f y x
let EuclidianDistance (p1: Pair<int, int>) (c2: Pair<int, int>) : float =
    let sqr x = x * x 
    let subsqr x = ReverseArgs (-) x >> sqr
    (Pair.HorizontalMap2 subsqr subsqr p1 >> Pair.Deconstruct (+) >> float >> sqrt) c2

//Or multiplication of `fractions`
let FractionMultiplication =
    Pair.HorizontalMap2 (*) (*)

//or Division of `fractions`... by multiplying!
let FractionDivision =
    Pair.DiagonalMap2 (*)(*)