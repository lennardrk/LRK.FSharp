module Other

open LRK.Datastructures


let array : Opt<int>[] = Array.map(Opt.Some)([1..5000] |> List.toArray)

let incr = (+) 1
let sqr x = x * x

//This kind of composability is possible with every other monad, which makes it quite powerfull to use.
let transformOptArray = Array.map(Opt.Map sqr >> Opt.Map incr >> Opt.Map float >> Opt.Map sqrt )
let transformOptArray2 = Array.map(Opt.Map(sqr >> incr >> float >> sqrt)) //Same, but with less steps 

let transformedArray =  transformOptArray array
let transformedArray2 =  transformOptArray2 array