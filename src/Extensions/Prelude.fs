namespace Prelude.Extensions

[<AutoOpen>]
module Prelude =
    /// Flips the order of the arguments.
    let inline flip f a b = f b a

    /// Returns the first argument always.
    let inline always a _ = a

    /// Place holder for unimplemented code.
    let todo<'a> : 'a =
        "Not implemented"
        |> System.NotImplementedException
        |> raise

    /// Applies `f` to `a` and returns `a`.
    let inline tap f a =
        f a
        a
