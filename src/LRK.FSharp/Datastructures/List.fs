//-----------------------------------------------------------------------
// Copyright © 2020-2022 - lennardrk <lennard@lennardrk.nl>
//-----------------------------------------------------------------------

namespace LRK.Datastructures

///<summary>
/// Example of a list structure.
/// It is not compiled into the library. As with other types, native f# types should be prefered...
///</summary>
#nowarn "40"
type CList<'a> =
    |Node of Pair<'a, CList<'a>>
    |Empty

    static member Unit (x: 'a) (y : CList<'a>) : CList<'a>  =
        CList<'a>.Node(Pair<'a,CList<'a>>.Unit x y)

    static member MkCList (x: 'a) : CList<'a> =
        CList<'a>.Unit (x) (CList<'a>.Empty)

    static member Map<'b> (f: 'a -> 'b ) (cl : CList<'a>) : CList<'b> =
        let rec g : CList<'a> -> CList<'b> = 
            let h (l: CList<'a>) = 
                match l with
                |Empty -> CList<'b>.Empty
                |Node n -> CList<'b>.Unit (f n.X) (g n.Y)
            h
        g cl

    static member Join (cl : CList<CList<'a>>) :  CList<'a> =
        let rec g : CList<CList<'a>> -> CList<'a> = 
            let h (l: CList<CList<'a>>) = 
                match l with
                |Empty  -> Empty
                |Node t -> (g t.Y).Append(t.X)
            h
        g cl

    static member Bind<'b> (f: 'a -> CList<'b>) (cl: CList<'a>) :  CList<'b> =
        (CList<'a>.Map<CList<'b>> f >> CList<'b>.Join) cl

    static member Concat (l1: CList<'a>) (cl2 : CList<'a>) : CList<'a> =
        let rec g (l2: CList<'a>) : CList<'a> =
            match l2 with
            |Node n -> CList<'a>.Unit n.X (g n.Y)
            |Empty -> l1
        g cl2

    static member (+) (l1 : CList<'a>, l2: CList<'a>) =
        CList.Concat l1 l2

    static member (+) (l : CList<'a>, a: 'a) =
        l + CList.MkCList(a)

    static member Aggregate<'b> (aggregator: 'b)( f: Pair<'b,'a> -> 'b) (cl : CList<'a>) : 'b =
        let rec g  : Pair<'b, CList<'a>> -> 'b =
            let h (t: Pair<'b, CList<'a>>) =
                match t.Y with
                |Empty -> t.X
                |Node n -> g(Pair.Unit (f(Pair.Unit t.X n.X)) n.Y)
            h
        g(Pair.Unit aggregator cl)

    static member WhereClist (pred: 'a -> bool)(cl : CList<'a>) : CList<'a>  =
        let filter (t : Pair<CList<'a>,'a>) =
            if pred t.Y 
            then (CList<'a>.MkCList >> (+)) t.Y t.X
            else t.X
        CList<'a>.Aggregate<CList<'a>>(CList<'a>.Empty)(filter) cl

    static member FromSeq (s : seq<'a>) : CList<'a> =
        let rec g : Pair<seq<'a>, CList<'a>> -> CList<'a> =
            let h (t : Pair<seq<'a>, CList<'a>>) = 
                if Seq.isEmpty t.X
                then t.Y
                else g (Pair.Unit (Seq.skip 1 t.X) (CList<'a>.MkCList(Seq.head t.X) + t.Y))
            h
        g(Pair.Unit s CList.Empty)

    static member FromArray(a: 'a[]) : CList<'a> =
        CList<'a>.FromSeq(a :> seq<'a>)

    static member CountCList (cl : CList<'a>) : int =
        let counter (i : Pair<int, 'a>) = 
            i.X + 1 
        CList<'a>.Aggregate<int>(0)(counter) cl

    member this.Then<'b>(f: 'a -> CList<'b>) =
        (CList<'a>.Bind<'b> f) this

    member this.Append(c : CList<'a>) : CList<'a> =
        this + c

    member this.Head() : Opt<'a> =
        match this with
        |Empty  -> Opt<'a>.MkNone()
        |Node n -> Opt<'a>.Some n.X

    member this.Where(pred : 'a -> bool) : CList<'a> =
        CList<'a>.WhereClist pred this

    member this.Count() : int =
        CList<'a>.CountCList this

    member this.Iter : seq<'a> =
        let rec cListIterator (l: CList<'a>) : seq<'a> = seq {
            match l with
            |Node n -> yield n.X
                       yield! cListIterator(n.Y)
            |Empty -> ()
        }
        cListIterator this
