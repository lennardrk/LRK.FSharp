//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Functional

open LRK

/// NOTE:
/// This was the first version of my coroutine. It's currently kept for reference puroposes. 

//Several type alias for modeling 
type OldCoroutine<'s, 'e, 'a> =  Fun<'s,Either<NoRes<'s,'e,'a>, Pair<'a,'s>>>
and  NoRes<'s,'e,'a> =        Either<'e, Continuation<'s,'e,'a>>
and  Continuation<'s,'e,'a> = CON of Pair<'s, OldCoroutine<'s,'e,'a>> // Cyclic dependencies through type abbrivations aren't supported by f#. Making it a discriminated union is a workaround for this.

module OldCoroutineFlow =

    //Helper to downcast the union for continuation (workaround)
    let DowncastCont<'s,'e,'a> (c: Continuation<'s,'e,'a>) : Pair<'s, OldCoroutine<'s,'e,'a>> =  
        match c with
        |CON c1 -> c1

    //Function signature
    let Cor<'s,'e,'a> (f : 's -> Either<NoRes<'s,'e,'a>, Pair<'a,'s>>) : OldCoroutine<'s,'e,'a> =
        f
    
    //Constructor
    let Unit<'s,'e,'a> (a : 'a) : OldCoroutine<'s,'e,'a> = 
        Cor (Pair.Unit a >> Either.Right)

    //Apply a continuation
    let apply<'s,'e,'a> (v : Pair<'s,OldCoroutine<'s,'e,'a>>)  = 
        v.Y v.X

    //Construct an error
    let MkError<'s,'e,'a> (error : 'e) : Either<NoRes<'s,'e,'a>, Pair<'a,'s>> = 
        (Either.Left >> Either.Left) error

    //Joins a nested coroutine
    let rec Join<'s,'e,'a> (cc : OldCoroutine<'s,'e, OldCoroutine<'s,'e,'a>>) : OldCoroutine<'s,'e,'a> = //TO-DO: Refactor naar een mapper, net als bij de Map onder deze functie
        let f (s : 's) =
            match cc s with
            |Left nores -> match nores with
                           |Left error -> MkError error
                           |Right cont -> (Pair.MapRight(Join) >> apply) (DowncastCont cont)
            |Right lastres -> (Pair.Flip >> apply) lastres
        f
    
    //Maps a coroutine
    let rec Map<'s,'e,'a,'b> (f: 'a -> 'b) (c : OldCoroutine<'s,'e,'a>):  OldCoroutine<'s,'e,'b> =
        let mapper =
            let onContinuation = DowncastCont >> Pair.MapRight(Map f) >> apply
            let onNoRes = Either.Map MkError onContinuation >> Either.Deconstruct id id
            let onRes = Pair.MapLeft f
            Either.Map onNoRes onRes >> Either.Deconstruct id Either.Right
        fun s -> mapper (c s)

    let Bind<'s,'e,'a,'b> (c : OldCoroutine<'s,'e,'a>) (f : 'a -> OldCoroutine<'s,'e,'b>) : OldCoroutine<'s,'e,'b> =
        (Map f >> Join) c

    let GetCorState : OldCoroutine<'s,'e,'s> =
        let h s = Either.Right (Pair.Unit (s) (s))           
        h

    let SetCorState (s : 's): OldCoroutine<'s,'e,unit> =
        let h _ = Either.Right (Pair.Unit () (s))
        h
    
    // With the suspend function you can store the current state together with the coroutine in a tuple (a continuation), so you can apply the state to the cor later.
    // in the mean time you can do other things to the coroutine like getting/settings states and transform a, while the old coroutine remains intact.
    let Suspend<'s,'e, 'a> (c : OldCoroutine<'s,'e,'a>) : OldCoroutine<'s,'e,'a> =
        let f (s : 's) = 
            (Either.Right >> Either.Left) <| Continuation.CON(Pair.Unit(s)(c))
        f

    let Resume<'s,'e,'a> (c: OldCoroutine<'s,'e,'a>) : OldCoroutine<'s,'e,'a> =
        Map id c

    //TODO:
    // A method to fail a coroutine
    // A try catch coroutine
    // 