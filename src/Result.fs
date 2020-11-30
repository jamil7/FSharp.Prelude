namespace FSharp.Prelude.Operators.Result

[<AutoOpen>]
module ResultOperators =
    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (result: Result<'a, 'e>): Result<'b, 'e> = Result.map f result

    /// Infix apply operator.
    let inline (<*>) (f: Result<('a -> 'b), 'e>) (result: Result<'a, 'e>): Result<'b, 'e> =
        match f, result with
        | Ok fOk, Ok resOk -> Ok(fOk resOk)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    /// Infix bind operator.
    let inline (>>=) (f: 'a -> Result<'b, 'e>) (result: Result<'a, 'e>): Result<'b, 'e> = Result.bind f result


namespace FSharp.Prelude

open FSharp.Prelude.Operators.Result

[<RequireQualifiedAccess>]
module Result =
    /// Wraps a value in an Ok Result.
    let singleton (value: 'a): Result<'a, 'e> = Ok value

    let apply (f: Result<('a -> 'b), 'e>) (result: Result<'a, 'e>): Result<'b, 'e> = f <*> result

    let bindError (f: 'e1 -> Result<'a, 'e2>) (result: Result<'a, 'e1>): Result<'a, 'e2> =
        match result with
        | Ok ok -> Ok ok
        | Error error -> f error

    let map2 (f: 'a -> 'b -> 'c) (result1: Result<'a, 'e>) (result2: Result<'b, 'e>): Result<'c, 'e> =
        f <!> result1 <*> result2

    let andMap (result: Result<'a, 'e>) (f: Result<('a -> 'b), 'e>): Result<'b, 'e> = map2 (|>) result f

    let sequence (results: Result<'a, 'e> list): Result<'a list, 'e> =
        List.foldBack (fun head tail -> (fun head tail -> head :: tail) <!> head <*> tail) results (singleton [])

    let zip (result1: Result<'a, 'e>) (result2: Result<'b, 'e>): Result<'a * 'b, 'e> =
        (fun a b -> a, b) <!> result1 <*> result2

    let ofOption (error: 'e) (option: 'a option): Result<'a, 'e> =
        match option with
        | Some thing -> Ok thing
        | None -> Error error

    let ofChoice (choice: Choice<'a, 'e>): Result<'a, 'e> =
        match choice with
        | Choice1Of2 left -> Ok left
        | Choice2Of2 right -> Error right

[<AutoOpen>]
module ResultCE =
    type ResultBuilder() =
        member _.Return(value: 'a): Result<'a, 'e> = Result.singleton value

        member _.ReturnFrom(result: Result<'a, 'e>): Result<'a, 'e> = result

        member _.Zero(): Result<unit, 'e> = Result.singleton ()

        member _.Bind(result: Result<'a, 'e>, f: 'a -> Result<'b, 'e>): Result<'b, 'e> = Result.bind f result

        member _.Delay(f: unit -> Result<'a, 'e>): unit -> Result<'a, 'e> = f

        member _.Run(f: unit -> Result<'a, 'e>): Result<'a, 'e> = f ()

        member _.Combine(result: Result<unit, 'e>, f: unit -> Result<'a, 'e>): Result<'a, 'e> = Result.bind f result

        member _.TryWith(f: unit -> Result<'a, 'e>, g: exn -> Result<'a, 'e>): Result<'a, 'e> =
            try
                f ()
            with exn -> g exn

        member _.TryFinally(f: unit -> Result<'a, 'e>, g: unit -> unit): Result<'a, 'e> =
            try
                f ()
            finally
                g ()

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> Result<'a, 'e>): Result<'a, 'e> =
            try
                (fun () -> f disposable) ()
            finally
                (fun () ->
                    if not (obj.ReferenceEquals(disposable, null))
                    then disposable.Dispose()) ()

        member _.BindReturn(result: Result<'a, 'e>, f: 'a -> 'b): Result<'b, 'e> = Result.map f result

        member _.MergeSources(result1: Result<'a, 'e>, result2: Result<'b, 'e>): Result<'a * 'b, 'e> =
            Result.zip result1 result2
