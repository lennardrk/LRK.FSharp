//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Datastructures

///Option Structure
[<Struct>]
type Opt<'a> =
    | Some of 'a
    | None

    /// <summary>Constructor method for Opt></summary>
    static member MkNone () : Opt<'a> =
        Opt.None

    static member Unit a : Opt<'a> = 
        Opt<'a>.Some a

    // TODO: MapDefault for Opt is currently private. Consider making it public to facilitate a natural transformation 
    /// <summary>Peforms a natural transformation for opt.</summary>
    /// <returns>On Opt.Somefunction <paramref name="f"/> is used to transform a into b, otherwise function g is used to provide a default value.</returns>
    static member private MapDefault<'b> (f : 'a -> 'b) (g: unit -> 'b) (a : Opt<'a>) : 'b =
        match a with
        |None -> g()
        |Some v -> f v

    /// <summary>Deconstructs Opt.</summary>
    /// <returns>When Opt is none, f will be used to generate the desired output.</returns>
    static member Deconstruct (f: unit -> 'a)(o : Opt<'a>) :  'a =
        Opt<'a>.MapDefault<'a>(id)(f) o

    /// <summary>Creates an endofunctor for Opt.
    ///     <p>On Opt.Some the transformation takes place and on None it remains None.</p>
    /// </summary>
    static member Map<'b> (f : 'a -> 'b) (o : Opt<'a>): Opt<'b> =
        (Opt.MapDefault (f >> Opt.Some) (Opt.MkNone)) o

    ///<summary>Joins a nested Opt.</summary>
    ///<returns>Opt.Some when the inner and outer Opt are both of type <see cref="Opt{a}.Some"/></returns>
    static member Join (o: Opt<Opt<'a>>): Opt<'a> =
        (Opt.MapDefault id Opt.MkNone) o

    ///<summary>Kleisli composition bind on Opt.</summary>
    static member Bind<'b> (f: 'a -> Opt<'b>) (o : Opt<'a>) : Opt<'b> =
        (Opt.Map f >> Opt.Join) o

    ///<summary>Utility function to make kleisli compositions on two Opts.</summary>
    static member Bind2<'b,'c> (f: 'a -> 'b -> Opt<'c>) (o1 : Opt<'a>) (o2 : Opt<'b>)  : Opt<'c> =
        let f1 x1 =
            let f2 x2 = 
                f x1 x2
            Opt.Bind(f2) o2
        Opt.Bind(f1) o1
    
    ///<summary>Utility function to make kleisli compositions on two Opts.</summary>
    static member Map2<'b,'c>(f: 'a -> 'b -> 'c)(o1: Opt<'a>)(o2: Opt<'b>) : Opt<'c> =
        let f1 x1 =
            let f2 x2 =
                (f x1 >> Opt.Some) x2
            Opt.Bind(f2) o2
        Opt.Bind(f1) o1
