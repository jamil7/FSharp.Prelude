namespace Prelude

[<AutoOpen>]
module FunctionalExtensions =
    /// Flips the order of the arguments.
    let inline flip (f: 'a -> 'b -> 'c) (a: 'b) (b: 'a) : 'c = f b a

    /// Returns the first argument always.
    let inline always (a: 'a) (_: 'b) : 'a = a

    /// Place holder for unimplemented code.
    let todo<'a> : 'a =
        "Not implemented"
        |> System.NotImplementedException
        |> raise

    /// Applies `f` to `a` and returns `a`.
    let inline tap (f: 'a -> unit) (a: 'a) : 'a =
        f a
        a
