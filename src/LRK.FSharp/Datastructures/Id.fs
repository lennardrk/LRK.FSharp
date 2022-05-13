//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Datastructures

type Id<'a> = 'a
type Fun<'a,'b> = 'a -> 'b


module Id =
    
    /// <summary>
    /// Wraps the generic type
    /// </summary>
    /// <param name="a"></param>
    let Unit<'a> (a : 'a):  Id<'a> =
        a

    /// <summary>
    /// Unwraps the contained generic type
    /// </summary>
    /// <param name="a"></param>
    let Deconstruct (a: Id<'a>) : 'a = 
        a

    /// <summary>
    /// A mapping function
    /// </summary>
    /// <param name="f">The function that transforms the generic type</param>
    /// <param name="a">The argument that will be tranformed by the given function</param>
    let Map<'a,'b> (f: 'a -> 'b) (a : Id<'a>): Id<'b> =
        f a

    /// <summary>
    /// Joins a nested Id
    /// </summary>
    /// <param name="a">The nested Identity to be joined</param>
    let Join<'a> (a : Id<Id<'a>> ) : Id<'a> =
        a

    /// <summary>
    /// Kleisli composition for id
    /// </summary>
    let BindId<'a,'b> (f : 'a -> Id<'b>) (a : Id<'a>) : Id<'b> =
        (Map f >> Join) a
