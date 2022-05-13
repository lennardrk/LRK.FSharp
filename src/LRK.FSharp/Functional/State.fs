//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Functional

open LRK.Datastructures

type State<'s,'a> =
    's -> Pair<'a,'s>

module State =  
    let State<'s,'a>(f: 's -> Pair<'a,'s>) : State<'s,'a> = 
        f

    /// Constructor of state. Embeds a value into a function.
    let Unit<'s,'a> (x : 'a) : State<'s,'a> = 
        let h (s : 's) = Pair.Unit x s
        h

    /// Polymorphic endofunctor for State
    let Map<'s,'a,'b> (f: 'a -> 'b) (s : State<'s,'a>) : State<'s,'b> =
        s >> Pair.MapLeft f
    
    let private apply<'a,'b> (t : Pair<('a -> 'b), 'a>) : 'b =
        t.X t.Y

    ///Joins a nested state by applying the state.
    let Join<'s,'a> (s : State<'s, State<'s,'a>>) :  State<'s,'a> =    
        s >> apply

    ///Kleisli compositio on State
    let Bind<'s,'a,'b> (f: 'a -> State<'s,'b>) (s: State<'s,'a>) : State<'s,'b> =
        (Map f >> Join) s

    let Bind2<'s,'a,'b,'c > (f: 'a -> 'b -> State<'s,'c>) (s1: State<'s,'a>) (s2: State<'s,'b>) : State<'s,'c> =
        let f1 a1 =
            let f2 a2 = 
                f a1 a2
            Bind f2 s2
        Bind f1 s1

    let Map2<'s,'a,'b,'c > (f: 'a -> 'b -> 'c) (s1: State<'s,'a>) (s2: State<'s,'b>) : State<'s,'c> =
        let f1 a1 =
            let f2 a2 = 
                (f a1 >> Unit) a2
            Bind f2 s2
        Bind f1 s1

    ///Gets the statefull value of State. Use in combination with Bind.
    let GetState<'s> : State<'s,'s> = 
        let h (s: 's) = Unit<'s,'s> s s
        h

    ///Embeds a state value in a new state. Use in combination with bind.
    let SetState<'s> (s : 's) : State<'s,unit> = 
        let h _ = Unit () s
        h
