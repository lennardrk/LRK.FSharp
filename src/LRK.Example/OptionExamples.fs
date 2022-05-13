module OptionExamples

open System
open LRK.Datastructures

// Opt is somewhat like 'Nullable'
// Creation an Opt (abbrv. of Option)
let optOne  : Opt<int> = Opt.Unit 1
let optNone : Opt<int> = Opt<int>.MkNone()

//Deconstructing an opt. When its none, it will use the supplied function to provide value.
let one : int = Opt.Deconstruct(fun _ -> 1) optOne

//A 'regular' map. When none, nothing happens, but the type does change.
let mapped : Opt<int> -> Opt<string> = 
    Opt<int>.Map(Convert.ToString)

//Joining two nested opts
let nestedOpt : Opt<Opt<int>> = Opt.Unit optOne
let nestedNoneOpt : Opt<Opt<int>> = Opt.Unit (Opt.MkNone());

let joinedOpt : Opt<int> = Opt<int>.Join nestedOpt // will result in a Some of 1
let joinedNoneOpt : Opt<int> = Opt<int>.Join nestedNoneOpt // Will result in a None.

//Bind on Opt. It can become quite usefull when used right.
//If the opt is none, the result is always none.
let ParseInt : Opt<string> -> Opt<int> = Opt.Bind(fun x ->
    let mutable parsed : int = 0 //Note that mutability is actually bad practice. In this case its the only safe way to parse ints
    if Int32.TryParse(x, &parsed) then 
        Opt<int>.Unit parsed
    else
        Opt<int>.None
)

// Or a double bind for 'safe' division
// If one of two opts is none, the result is always none.
let SafeDivision = Opt<float>.Bind2<float, float>(fun x y ->
    if y = 0 then 
        Opt<float>.None
    else
        Opt<float>.Unit (x / y)
)
