module StateExamples

open LRK.Datastructures
open LRK.Functional
open OptionExamples

// Monads like State, Process and Coroutine are a bit odd.
// They aren't exactly structures like Option or List, but rather describe the tranformation of functions.
// Though, they do behave like a data structure.

//Parsing a number from a string.
let One = State.Unit<unit, int> 1
let Increment<'a> = State.Map<'a, int, int>((+) 1)
let Two = Increment One //Nothing actually has been calculated, yet. Only the function got transformed. In its current state, it just means `1 + 1` The actual exection takes place when you apply One()

//Bind, as with other monads, needs you to return another state. This makes it a usefull pattern , since you can decide the next state that will be performed, and might do something very different entirely.
// In order to make it usable, you need to make use of 's, which can be seen as the memory or stack of your state.
//I.E. if you want to parse a number:
//(Do not supply a function such as 'a -> 's -> Pair<'a,'s> to Bind. It defeats the purpose of the structure preserving safety of State)
let TryParseInt : State<Opt<string>, unit> -> State<Opt<string>, Opt<int>> = 
    State.Bind<Opt<string>, unit, Opt<int>>(fun _ ->
        State.Bind(ParseInt >> State.Unit) State.GetState //ParseInt is from OptionExamples
    )

let ParsableString = State.SetState (Opt.Unit "24323")
let ParsedString : State<Opt<string>, Opt<int>> = TryParseInt ParsableString

//Note that it not possible to change to type of 's. It's not meant to transform.
let ChangeState =
    State.Bind(fun x ->
        // This Compiles
        State.Bind(fun _ ->
            // Whatever you want to happen next...
            State.Unit x
        ) (State.SetState(Opt.Unit ""))

        //// But this does not
        //State.Bind(fun _ ->
        //    State.Unit x
        //) (State.SetState(""))
    ) (ParsedString)

// To make 's more versatile, you could use an immutable dictionary like Map<'Key, 'Value> as your stack for State<'s,'a>.
// It's currenty not implemented, but it's on my to-do list for implementing it in coroutines.

//If you want to access 's and 'a at the same time, you could use Bind2 or Map2.
// Map2 can be used if you don't want to return a State yourself (It is done inside Map2).
let format = Opt.Map2(fun a s -> sprintf "%i, %s" a s)
let bind2Ex = State.Map2(format)(ParsedString)(State.GetState)

// CoroutineExamples.fs had some more examples. It's somewhat similair to State, but more advanced.