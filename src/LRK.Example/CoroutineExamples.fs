module CoroutineExamples

open System
open LRK.Datastructures
open LRK.Functional
open LRK.Functional.Coroutine

// General examples of Coroutine functions
// Currently exceptions are left out, because it's not implemented properly yet.

// Embedding a value in coroutine:
let One = Unit<int, unit, int> 0
//Mapping a coroutine
let Increment= Map<int, unit, int, int>((+) 1)
let Two = Increment One //Same with as with State, only the function is transformed and not yet executed.

let AddFromCor = Bind<int, unit, int, int> ( fun a ->
    Bind( fun s ->
        Unit (s + a)
    ) GetState
)

//This coroutine essentially still contains two, but when you apply the state it adds whatever the state was to two
let TwoPlusState : Coroutine<int,unit, int> = AddFromCor Two 

//The same could be rewritten with Bind2
let AddFromCor2 : Coroutine<int, unit, int>= 
    Bind2(fun a s ->
        Unit(a + s)
    ) (Two) (GetState)

//The result will be a Continuation.Done, consisting of a Pair where x: 4 (value) and y: 2 (state)
let res = AddFromCor2 2

// Combine
// Sometimes you one to ignore whatever the last result was and continue with something else. That is exactly what combine does.
// It composes the first coroutine with the second, passing on the state properly and it just returns second coroutine as the next step while binding to the first.
let firstRoutine : Coroutine<string, unit, string> = Unit "First"
let secondRoutine : Coroutine<string, unit, int> = Unit 2

let combinedRoutines : Coroutine<string, unit, int> = Combine firstRoutine secondRoutine //The result would be `2`, while keeping the same state from the first.

//More advanced examples.
//Summing a list
let rec SumCor (summable :  Coroutine<int,unit, int list>) : Coroutine<int, unit, unit> =
   Bind2(fun (list: int list) (sum: int) -> 
       if list.IsEmpty then
           Freeze
       else
           SumCor(SetState (sum + list.Head) >>= Unit list.Tail)
   ) summable GetState

let Sum (list : int list) : int =
    let initial = Unit list
    RunUntilEnd (SumCor initial) 0 


//// Equivalent radix sort without coroutines
//let Radix (array : uint[]) : uint[] =
//    let iters = Array.max(array) |> (double >> log10 >> (+) 1.0 >> int)
//    let prefixSum = (Array.scan (+) 0) >> Array.skip(1)

//    let rec g (array : uint[]) (iter: int) : uint[] =
//        if iter <= iters then
//            let divisor = (pown (10) (iter - 1)) |> uint
//            let digit num = ((num / divisor) % 10u) |> int

//            let prefixSum = 
//                Array.fold (fun (x : int[]) -> fun (y: uint) -> 
//                    let res = digit y
//                    x.[res] <- x.[res] + 1
//                    x
//                ) (Array.create 10 0) array |> prefixSum

//            let res = 
//                Array.foldBack<uint, uint[] * int[]>(fun item cumu ->
//                    let currentDigit = digit item
//                    (snd cumu)[currentDigit] <- (snd cumu)[currentDigit] - 1
//                    (fst cumu)[(snd cumu)[currentDigit]] <- item
//                    cumu
//                )(array)(Array.create<uint> (array.Length) 0u, prefixSum)

//            g (fst res) (iter + 1)
//        else
//            array
//    g array 1

//let res = Radix arr
//printfn "%b" (isAscending res)

let arrayLength = 500
let rnd = Random()
let unsortedArray = [for i in 0..arrayLength do rnd.Next(99999) |> uint] |> List.toArray
let isAscending l = l |> Seq.pairwise |> Seq.forall (fun (a, b) -> a <= b)

let RadixSort (array: uint[]) =
    let maxIter = Array.max array |> (double >> log10 >> (+) 1.0 >> int)
    let prefixSum = Array.scan (+) 0 >> Array.skip 1

    let rec g (cor : Coroutine<int,unit,uint[]>)  = 
        let corBinding currentArray currentIter =
            if currentIter <= maxIter then
                let divisor = pown (10) (currentIter - 1) |> uint
                let digit num = (num / divisor) % 10u |> int

                let countCor = 
                    Map<int, unit, uint[], int[]>(Array.fold (fun (x : int[]) (y: uint) -> 
                        let res = digit y
                        x.[res] <- x.[res] + 1
                        x
                    )(Array.create 10 0))(Unit currentArray)
                let prefixSumCor = Map prefixSum countCor

                let sortf prefixSumArray = 
                    Map(Array.foldBack<uint, uint[] * int[]>(fun item cumu ->
                        let currentDigit = digit item
                        (snd cumu)[currentDigit] <- (snd cumu)[currentDigit] - 1
                        (fst cumu)[(snd cumu)[currentDigit]] <- item
                        cumu
                    ) currentArray)(Unit (Array.create<uint> (array.Length) 0u, prefixSumArray))
                let sortCor = Map(fst)(Bind sortf prefixSumCor)
                //g(Combine (SetState(currentIter + 1)) sortCor)
                g(SetState(currentIter + 1) >>= sortCor)             
            else
                Unit currentArray   
        Bind2(corBinding) cor GetState
    g (Unit<int,unit,uint[]> array)

let result = 
    Bind<int, unit, uint[], unit>(fun l ->
        printfn "%b" (isAscending l)
        printfn "%A" l
        Freeze
    )(RadixSort unsortedArray) 1
