//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Datastructures
///The Either structure
[<Struct>]
type Either<'a, 'b> =
    | Left of Left: 'a
    | Right of Right: 'b

    /// <summary>Right side constructor, putting 'a in right</summary>
    static member Unit(a: 'a): Either<'b, 'a> = Either<'b, 'a>.Right a

    /// <summary>Deconstruct Either</summary>
    static member Deconstruct<'c> (f: 'a -> 'c) (g: 'b -> 'c) (e: Either<'a, 'b>): 'c =
        match e with
        | Left a -> f a
        | Right b -> g b

    /// <summary>Endofunctor for Either</summary>
    /// <param name="f">Left side function</param>
    /// <param name="g">Right side function</param>
    /// <param name="e">Either to be transformed</param>
    /// <returns></returns>
    static member Map<'a1, 'b1> (f: 'a -> 'a1) (g: 'b -> 'b1) (e: Either<'a, 'b>): Either<'a1, 'b1> =
        let leftMap = f >> Either<'a1, 'b1>.Left
        let rightMap = g >> Either<'a1, 'b1>.Right
        match e with
        | Left  a -> leftMap a
        | Right b -> rightMap b

    /// <summary>Endofunctor for Either</summary>
    /// <returns>The un-nested either</returns>
    static member Join(ee: Either<Either<'a, 'b>, Either<'a, 'b>>)  : Either<'a, 'b> =
        match ee with
        | Left a ->  a
        | Right ab -> ab

    /// <summary>Bind on either to achieve kleisli composition</summary>
    /// <param name="f">Binding function on the left side</param>
    /// <param name="g">Binding function on the right side</param>
    /// <param name="e">Either</param>
    /// <returns>Transformed Either</returns>
    static member Bind<'c,'d> (f: 'a -> Either<'c, 'd>) (g: 'b -> Either<'c, 'd>) (e: Either<'a, 'b>): Either<'c, 'd> =
        (Either.Map f g >> Either.Join) e

    /// <summary>Utility function to make kleisli compositions on two Either.</summary>
    /// <returns>Transformed either...</returns>
    static member Bind2<'c,'d,'e,'f>  
        (f1 : 'a -> 'c -> Either<'e,'f>) (f2 : 'a -> 'd -> Either<'e,'f>)
        (f3 : 'b -> 'c -> Either<'e,'f>) (f4 : 'b -> 'd -> Either<'e,'f>)
        (e1 : Either<'a,'b>) (e2 : Either<'c,'d>) : Either<'e,'f> =
            let a x1 =
                Either.Bind (f1 x1) (f2 x1) e2
            let b x1 =
                Either.Bind (f3 x1) (f4 x1) e2

            Either.Bind a b e1

    /// <summary>Swaps type arguments for Either</summary>
    /// <returns></returns>
    static member Swap(e: Either<'a, 'b>): Either<'b, 'a> =
        match e with
        | Left a -> Either.Right a
        | Right b -> Either.Left b
