//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Datastructures

open LRK.Helpers

/// <summary>
/// The Pair structure.
/// </summary>
[<Struct>]
type Pair<'a,'b> private(x : 'a, y :'b) =
    member _.X = x
    member _.Y = y

    /// <summary>
    ///     Constructor for the Pair structure.
    /// </summary>
    /// <param name="x">The left parameter</param>
    /// <param name="y">The right parameter</param>
    static member Unit x y : Pair<'a,'b> =
        Pair<'a,'b>(x,y)
    
    /// <summary>
    ///    Left side constructor.
    /// </summary>
    /// <param name="x">The left parameter</param>
    static member UnitL x : Pair<'a,unit> = 
        Pair<'a,unit>.Unit(x)()

    /// <summary>
    ///    Right side constructor.
    /// </summary>
    /// <param name="y">The right parameter</param>
    static member UnitR y : Pair<unit,'a> = 
        Pair<unit,'a>.Unit()(y)
    
    /// <summary>
    ///    Reversed constructor.
    /// </summary>
    /// <param name="x">The right parameter</param>
    /// <param name="y">The left parameter</param>
    static member UnitRL (x: 'a) (y: 'b) : Pair<'b,'a> =
        Pair.Unit(y)(x)

    /// <summary>
    ///    Left side deconstructor.
    /// </summary>
    /// <param name="p">The Pair</param>
    static member OutL (p : Pair<'a,'b>):  'a =
        p.X

    /// <summary>
    ///    Right side deconstructor.
    /// </summary>
    /// <param name="p">The Pair</param>
    static member OutR (p : Pair<'a,'b>):  'b =
        p.Y

    /// <summary>
    ///    Deconstructor (Bi-Functor)
    /// </summary>
    /// <param name="f">Bi-Functor</param>
    /// <param name="p">The Pair</param>
    static member Deconstruct<'c> (f : 'a -> 'b -> 'c) (p : Pair<'a,'b>) : 'c =
        let dec = ((f << Pair<'a,'b>.OutL) >> ((>>) Pair<'a,'b>.OutR))
        dec p p
        
    /// <summary>
    /// A natural transformation to a native tuple.
    /// </summary>
    /// <param name="t">The tuple which you want to convert.</param>
    static member ToNativeTuple (t: Pair<'a,'b>) : 'a * 'b =
        (t.X, t.Y)

    /// <summary>
    /// A Mapper funtion to create endofunctors for Pair (Bi-Functor)
    /// </summary>
    /// <param name="f1">Function to transform parameter 1</param>
    /// <param name="f2">Function to transform parameter 2</param>
    /// <param name="t">Value to be mapped by an endofunctor</param>
    static member Map<'c,'d> (f1: 'a -> 'c)(f2: 'b -> 'd)(t : Pair<'a,'b> ): Pair<'c,'d> =
        Pair.Unit ((Pair.OutL >> f1) t) ((Pair.OutR >> f2) t)

    /// <summary>
    /// A mapper function to create endofunctors for Pair which only operate on the left side of the Pair.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="v"></param>
    static member MapLeft<'c> (f: 'a -> 'c) (v : Pair<'a,'b>) : Pair<'c, 'b> =
        (Pair.Map f Id.Unit<'b>) v
        
    /// <summary>
    /// A mapper to create endofunctors for Pair which only operate on the right side of the Pair.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="t"></param>
    static member MapRight<'c> (f : 'b -> 'c)(t : Pair<'a,'b>) :  Pair<'a, 'c> =
        (Pair.Map Id.Unit<'a> f) t

    /// <summary>
    /// Joins the left side of the first Pair and the right side of the second Pair of a nested Pair.
    /// </summary>
    /// <param name="x"></param>
    static member JoinLR (x:  Pair<Pair<'a, _>, Pair<_,'b>>) : Pair<'a,'b> =
        Pair.Unit ((Pair.OutL >> Pair.OutL) x) ((Pair.OutR >> Pair.OutR) x)

    /// <summary>
    /// Joins the right side of the first Pair and the left side of the second Pair of a nested Pair.
    /// </summary>
    /// <param name="x"></param>
    static member JoinRL (x:  Pair<Pair<_, 'a>, Pair<'b,_>>) : Pair<'a,'b> =
        Pair.Unit ((Pair.OutL >> Pair.OutR) x) ((Pair.OutR >> Pair.OutL) x)

    /// <summary>
    /// Joins the left side of the first Pair and the left side of the second Pair of a nested Pair.
    /// </summary>
    /// <param name="x"></param>
    static member JoinLL (x:  Pair<Pair<'a, _>, Pair<'b,_>>) : Pair<'a,'b> =
        Pair.Unit ((Pair.OutL >> Pair.OutL) x) ((Pair.OutR >> Pair.OutL) x)

    /// <summary>
    /// Joins the right side of the first Pair and the right side of the second Pair of a nested Pair.
    /// </summary>
    /// <param name="x"></param>
    static member JoinRR (x:  Pair<Pair<_, 'a>, Pair<_,'b>>) : Pair<'a,'b> =
        Pair.Unit ((Pair.OutL >> Pair.OutR) x) ((Pair.OutR >> Pair.OutR) x)

    ///<summary>Kleisli composition bind on Pair. (LR)</summary>
    static member BindLR f g (t : Pair<'a,'b>) : Pair<'c,'d> =
        (Pair.Map f g >> Pair.JoinLR) t

    ///<summary>Kleisli composition bind on Pair. (RL)</summary>
    static member BindRL f g (t : Pair<'a,'b>) : Pair<'d,'c> =
        (Pair.Map f g >> Pair.JoinRL) t

    ///Flips around X an Y
    static member Flip(t : Pair<'a,'b>) : Pair<'b,'a> =
        Pair.Unit t.Y t.X

    ///<summary>Utility function to make kleisli compositions on two Pairs.</summary>
    static member Bind2<'c,'d, 'ac,'ad,'bc, 'bd,'acd,'bcd> f g h i j k (t1: Pair<'a,'b>)(t2: Pair<'c,'d>) : Pair<'acd,'bcd>=
        let l (a : 'a) : Pair<'acd, unit>=
            let l2 (c : 'c ) : Pair<'ac, unit>= 
                Pair.Unit(f a c)()
            let r2 (d : 'd) : Pair<unit, 'ad> = 
                Pair.Unit()(g a d)
            Pair.Deconstruct h (Pair.BindLR l2 r2 t2)

        let r (b : 'b) : Pair<_, 'bcd> = 
            let l2 (c : 'c) : Pair<'bc, unit> = 
                Pair.Unit(i b c)()
            let r2 (d : 'd) : Pair<unit ,'bd>= 
                Pair.Unit()(j b d)
            Pair.Deconstruct k (Pair.BindLR l2 r2 t2)

        Pair.BindLR l r t1

    ///<summary>Utility function to make kleisli compositions on two Pairs horizontally.</summary>
    static member HorizontalMap2<'c,'d, 'ac, 'bd> (f : 'a -> 'c -> 'ac) (g: 'b -> 'd -> 'bd) (t1: Pair<'a,'b>)(t2: Pair<'c,'d>) : Pair<'ac,'bd> =
        (Pair<'a,'b>.Bind2<'c,'d,'ac, unit, unit, 'bd, 'ac, 'bd> 
            (f) 
            (Discard2) 
            (Pair.Unit) 
            (Discard2) 
            (g) 
            (Pair.Unit)
        ) t1 t2

    ///<summary>Utility function to make kleisli compositions on two Pairs diagonally.</summary>
    static member DiagonalMap2<'c,'d, 'ad, 'bc> (f : 'a -> 'd -> 'ad) (g: 'b -> 'c -> 'bc) (t1: Pair<'a,'b>)(t2: Pair<'c,'d>) : Pair<'ad,'bc> =
        (Pair<'a,'b>.Bind2<'c,'d, unit, 'ad, 'bc, unit, 'ad, 'bc>
            (Discard2) 
            (f) 
            (Pair.UnitRL) 
            (g) 
            (Discard2) 
            (Pair.UnitRL) 
        ) t1 t2
     