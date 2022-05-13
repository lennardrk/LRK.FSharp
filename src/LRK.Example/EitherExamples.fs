module EitherExamples

open System.Numerics
open LRK.Datastructures

// Constructing Either - Is it an int or string?
let eitherIntOrStr : Either<int, string> = Either<int, string>.Left 1
let eitherString : Either<int, string> = Either<int, string>.Right "1" 

// Whatever it is, we can deconstruct it. Both side must result in the same type.
let deconstructor : Either<int, string> -> string = 
    Either<int, string>.Deconstruct<string>(string)(id)

let one = deconstructor eitherIntOrStr = deconstructor eitherString // will be equal

// Mapping on Either
let vector = Vector<int>([|1;2;3|], 0)
let eitherVect = Either<int,Vector<int>>.Right vector
let eitherInt = Either<int,Vector<int>>.Left 1
let mapper = Either<int,Vector<int>>.Map((*) 2)((*) 2)
let arrayMap = Array.map(mapper)

let arrayOfEither = arrayMap [|eitherVect;eitherInt;eitherVect;eitherInt|]

// Bind on either
let bindEither =  
    Either<int,Vector<int>>.Bind<double, Vector<double>>
        (fun x -> double x |> Either.Left)
        (fun y -> double (y.Item 1) |> Either.Left) //It doesn't have to be Either.Right. As long as it results in an Either

