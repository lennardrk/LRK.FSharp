//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Functional

open LRK.Datastructures

// TODO: Add faillure and try-catch logic for Errors. 
// TODO: Add 'Memory management' utilities for the Coroutine type. (in a seperate type or directly on coroutines...)
// TODO: Writing a polymorphic funtion to create natural transformations for coroutine.

module Coroutine =
    type Coroutine<'s,'e,'a> =  's -> Continuation<'s,'e,'a>
    and Continuation<'s,'e,'a> = Done of Result: Pair<'a,'s> 
                               | Pause of Next: Pair<'s,Coroutine<'s,'e,'a>>
                               | Error of Error: Pair<'e,'s>

    /// <summary>Base structure for Coroutine</summary>
    /// <param name="f">Function Signature</param>
    /// <typeparam name="'s">State</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <typeparam name="'a">Value</typeparam>
    /// <returns>The Coroutine</returns>
    let Coroutine<'s,'e,'a> (f : 's -> Continuation<'s,'e,'a>) : Coroutine<'s,'e,'a> =
        f

    /// <summary>Creates a Coroutine. Embeds 'a into the coroutine as the first result.</summary>
    /// <param name="a"></param>
    /// <typeparam name="'s">State</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <typeparam name="'a">Value</typeparam>
    /// <returns></returns>
    let Unit<'s,'e,'a> (a : 'a) : Coroutine<'s,'e,'a> =
        Coroutine(Pair.Unit a >> Continuation<'s,'e,'a>.Done)

    /// <summary>Applies a coroutine.</summary>
    /// <param name="p">A pair consisting of a state and a coroutine</param>
    /// <returns>The continuation of the applied coroutine</returns>
    let private Apply<'s,'e,'a> (p : Pair<'s,Coroutine<'s,'e,'a>>) =
        p.Y p.X

    /// <summary>A mapping for Coroutine. Providing the ability to create endofunctors for Coroutine.</summary>
    /// <param name="f">Transformation function</param>
    /// <param name="c">Coroutine that will be mapped</param>
    /// <typeparam name="'s">State</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <typeparam name="'a">Start type</typeparam>
    /// <typeparam name="'b">Target type</typeparam>
    /// <returns></returns>
    let rec Map<'s,'e,'a,'b> (f: 'a -> 'b) (c: Coroutine<'s,'e,'a>) : Coroutine<'s,'e,'b>  =
        let onDone = (Pair.MapLeft f >> Done)
        let onPause = (Pair.MapRight (Map f) >> Pause)
        let onError = Error

        let f (s :'s) =
            match c s with
            | Done d -> onDone d
            | Pause p -> onPause p
            | Error e ->  onError e
        f
    
    /// <summary>Joins two nested corutines together. Returns a new coroutine that resolves the nested coroutine.</summary>
    /// <param name="c">The nested coroutine</param>
    /// <typeparam name="'s">State</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <typeparam name="'a">Type a of the nested coroutine</typeparam>
    /// <returns>The un-nested coroutine</returns>
    let rec Join<'s,'e,'a> (c: Coroutine<'s, 'e, Coroutine<'s,'e,'a>>) : Coroutine<'s,'e,'a> =
        let f s =
            match c s with
            |Done d -> (Pair.Flip >> Apply) d
            |Pause p -> (Pair.MapRight Join >> Apply) p
            |Error e -> Error e
        f
    
    /// <summary>Bind to the result of a Coroutine (like a Promise) and making kleisli composition possible.</summary>
    /// <param name="f">The transformation function the binds to 'a.</param>
    /// <param name="c">The coroutine that will be transformed.</param>
    /// <typeparam name="'s">State.</typeparam>
    /// <typeparam name="'e">Exception.</typeparam>
    /// <typeparam name="'a">Start type.</typeparam>
    /// <typeparam name="'b">Target type.</typeparam>
    /// <returns>The transformed Coroutine.</returns>
    let Bind<'s, 'e,'a,'b> (f: 'a -> Coroutine<'s,'e,'b>) (c: Coroutine<'s,'e,'a>) : Coroutine<'s,'e,'b> =
        (Map f >> Join) c

    /// <summary>Binds two coroutines together. The first supplied coroutine is executing first (when you apply the resulting coroutine).</summary>
    /// <param name="f">The transformation function, taking the results of both coroutines as input.</param>
    /// <param name="c1">First Coroutine</param>
    /// <param name="c2">Second Coroutine</param>
    /// <typeparam name="'a">Input type from the first coroutine</typeparam>
    /// <typeparam name="'b">Input type from the second coroutine</typeparam>
    /// <typeparam name="'c">Target type</typeparam>
    /// <typeparam name="'s">State</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <returns></returns>
    let Bind2<'s,'e,'a,'b,'c> (f: 'a -> 'b -> Coroutine<'s,'e,'c>) c1 c2  =
        let f1 x1 =
            let f2 x2 =
                f x1 x2
            Bind f2 c2
        Bind f1 c1

    let Map2<'s,'e,'a,'b,'c> (f: 'a -> 'b -> 'c) c1 c2  : Coroutine<'s,'e,'c>=
        let f1 x1 =
            let f2 x2 =
                (f x1 >> Unit) x2
            Bind f2 c2
        Bind f1 c1

    /// <summary>Same as bind, but with reversed arguments.</summary>
    let Then<'s, 'e,'a,'b> (c: Coroutine<'s,'e,'a>) (f: 'a -> Coroutine<'s,'e,'b>) : Coroutine<'s,'e,'b> =
        Bind f c

    /// <summary>
    ///     Similair to bind. You bind to the result of Coroutine P, but you return coroutine K.
    ///     Meaning, you discard the result of P and continue with K.
    /// </summary>
    /// <param name="p">The first coroutine of which the result will be discarded.</param>
    /// <param name="k">The second coroutine that will be continuing.</param>
    let Combine p k = 
        Bind  (fun x -> k) (p)

    /// Operator for combine
    let (>>=) = Combine

    /// <summary>A coroutine that gets the state</summary>
    let GetState<'s,'e> : Coroutine<'s,'e,'s> =
        fun s -> Done(Pair.Unit s s)
  
    /// <summary>A coroutine that sets the state</summary>
    /// <param name="s">The state that will be set</param>
    /// <typeparam name="'s">Type of the state that will be set</typeparam>
    /// <typeparam name="'e">Exception</typeparam>
    /// <returns>A coroutine that sets the supplied state</returns>
    let SetState<'s,'e> (s: 's) : Coroutine<'s,'e,unit> = 
        fun _ -> Done(Pair.Unit () s)
  
    /// <summary>Pause the coroutine to resume it later. Paused coroutines are automatically resolved using Bind.</summary>
    /// <param name="c"></param>
    /// <typeparam name="'s"></typeparam>
    /// <typeparam name="'e"></typeparam>
    /// <typeparam name="'a"></typeparam>
    /// <returns>A suspended Coroutine</returns>
    let Suspend<'s,'e,'a> (c : Coroutine<'s,'e,'a>) : Coroutine<'s, 'e,'a> =
         let f s = Pair.Unit s c |> Pause
         f

    /// <summary>
    ///     A coroutine that only keeps the state and pauses execution. Resuming it just returns the identitiy of the state.
    ///     Usually this Coroutine is meant to be the last coroutine in the order of execution. It can still be mapped or be binded.
    /// </summary>
    /// <returns>The `frozen` Coroutine</returns>
    let Freeze<'s,'e> : Coroutine<'s,'e, unit> =
        let f s = Pair.Unit s (Unit()) |> Pause
        f
        
    /// <summary>Running a coroutine untill it's finished or failed. Turns an existing coroutine into another and ensures that all subsequent corountines are unpaused.</summary>
    /// <returns>The last known state</returns>
    let RunUntilEnd (c : Coroutine<'s,'e,'a>) (s : 's) : 's =
        match (Bind Unit c) s with
        |Done d -> d.Y
        |Pause p -> p.X
        |Error e -> e.Y
