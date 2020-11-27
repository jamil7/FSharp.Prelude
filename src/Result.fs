namespace FSharp.Prelude.Operators

[<AutoOpen>]
module ResultOperators =
    /// Infix map operator.
    let (<!>) (f: 'a -> 'b) (result: Result<'a, 'c>): Result<'b, 'c> = Result.map f result

    /// Infix apply operator.
    let (<*>) (f: Result<('a -> 'b), 'c>) (result: Result<'a, 'c>): Result<'b, 'c> =
        match f, result with
        | Ok fOk, Ok resOk -> Ok(fOk resOk)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    /// Infix bind operator.
    let (>>=) (f: 'a -> Result<'b, 'c>) (result: Result<'a, 'c>): Result<'b, 'c> = Result.bind f result

namespace FSharp.Prelude

open FSharp.Prelude.Operators

[<RequireQualifiedAccess>]
module Result =
    let singleton (value: 'a): Result<'a, 'b> = Ok value

    let apply (f: Result<('a -> 'b), 'c>) (result: Result<'a, 'c>): Result<'b, 'c> = f <*> result

    let bindError (f: 'a -> 'b) (result: Result<'c, 'a>): Result<'c, 'b> =
        match result with
        | Ok ok -> Ok ok
        | Error error -> Error(f error)

    let map2 (f: 'a -> 'b -> 'c) (result1: Result<'a, 'd>) (result2: Result<'b, 'd>): Result<'c, 'd> =
        f <!> result1 <*> result2

    let andMap (result: Result<'a, 'b>) (f: Result<('a -> 'c), 'b>): Result<'c, 'b> = map2 (|>) result f

    let andApply (result: Result<'a, 'b>) (f: Result<('a -> 'c), 'b>): Result<'c, 'b> = f <*> result

    let sequence (results: Result<'a, 'b> list): Result<'a list, 'b> =
        List.foldr (fun head tail -> List.cons <!> head <*> tail) (singleton []) results

    let zip (result1: Result<'a, 'b>) (result2: Result<'c, 'b>): Result<('a * 'c), 'b> =
        (fun a b -> a, b) <!> result1 <*> result2

    let ofOption (error: 'a) (option: 'b option): Result<'b, 'a> =
        match option with
        | Some thing -> Ok thing
        | None -> Error error

    let ofChoice (choice: Choice<'a, 'b>): Result<'a, 'b> =
        match choice with
        | Choice1Of2 left -> Ok left
        | Choice2Of2 right -> Error right

[<AutoOpen>]
module ResultCE =
    type ResultBuilder() =
        member _.Return(x) = Result.singleton x

        member _.ReturnFrom(m: Result<_, _>) = m

        member _.Zero() = Result.singleton ()

        member _.Bind(m, f) = f >>= m

        member _.Bind((_, error), f) = Result.bindError f error

        member _.Delay(f: unit -> Result<_, _>) = f

        member _.Run(f: unit -> Result<_, _>) = f ()

        member _.Combine(m: Result<unit, 'b>, f: unit -> Result<'a, 'b>) = f >>= m

        member _.TryWith(f: unit -> Result<'a, 'b>, g: exn -> Result<'a, 'b>) =
            try
                f ()
            with e -> g e

        member _.TryFinally(f: unit -> Result<'a, 'b>, g: unit -> unit) =
            try
                f ()
            finally
                g ()

        member _.Using(m: 'a :> System.IDisposable, f: 'a -> Result<'a, 'b>) =
            try
                fun () -> f m
            finally
                (fun () -> if not (obj.ReferenceEquals(m, null)) then m.Dispose()) ()

        member _.BindReturn(m, f) = f <!> m

        member _.MergeSources(m1, m2) = Result.zip m1 m2
