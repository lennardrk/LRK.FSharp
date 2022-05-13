//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Functional

open LRK.Datastructures

type Process<'s,'e,'a> = 
    's -> Either<'e, Pair<'a,'s>>

module ProcessFlow = 
    let Process<'s,'e,'a> (f: 's ->  Either<'e, Pair<'a,'s>>) : Process<'s,'e,'a> = 
        f

    let Unit<'s,'e,'a> (a : 'a) : Process<'s,'e,'a> =
        let h (s : 's) = Either.Unit <| Pair.Unit a s
        h
         
    let Map<'s,'e,'a,'b> (f: 'a -> 'b) (p: Process<'s,'e,'a>): Process<'s,'e,'b> = 
        p >> Either.Map (Id.Unit) (Pair.MapLeft f)

    let private apply<'a,'b> (t: Pair<('a -> 'b), 'a>) :  'b = 
        t.X t.Y

    let Join<'s,'e,'a> (pp : Process<'s,'e, Process<'s,'e,'a>>) : Process<'s,'e,'a>  = 
        pp >> (Either.Map Either.Left apply) >> Either.Join

    let Bind<'s,'e,'a,'b> (f: 'a -> Process<'s,'e,'b>) (p: Process<'s,'e,'a>)  : Process<'s,'e,'b> =
        (Map f >> Join) p

    let Bind2<'s,'e,'a,'b, 'c> (f: 'a -> 'b -> Process<'s,'e,'c>) p1 p2  : Process<'s,'e,'c> =
        let f1 x1 =
            let f2 x2 =
                f x1 x2
            Bind f2 p2
        Bind (f1) (p1)

    let Get<'s,'e> : Process<'s,'e,'s> = 
        let h s = Either.Right (Pair.Unit s s) 
        h
    
    let Set<'s,'e> (s : 's) : Process<'s,'e,unit> =
        let h _ = Either<Pair<unit,'s>,'e>.Unit (Pair.Unit()(s))
        h

    let Fail<'s,'e,'a> (e: 'e): Process<'s,'e,'a> =
        let h _ = Either.Left e
        h

    let TryCatch<'s,'e,'a> (p: Process<'s,'e,'a>)(c: Process<'s,'e,'a>) : Process<'s,'e,'a> = 
        let h s = 
            let onException _ = c s
            let catch = Either.Map onException Id.Unit >> Either.Deconstruct id Either.Right
            (p >> catch) s
        h
