namespace FSharp.Prelude.Operators.Result

[<AutoOpen>]
module ResultOperators =

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (result: Result<'a, 'e>) : Result<'b, 'e> = Result.map f result

    /// Infix apply operator.
    let inline (<*>) (f: Result<'a -> 'b, 'e>) (result: Result<'a, 'e>) : Result<'b, 'e> =
        match f, result with
        | Ok fOk, Ok resOk -> Ok(fOk resOk)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    /// Infix bind operator.
    let inline (>>=) (result: Result<'a, 'e>) (f: 'a -> Result<'b, 'e>) : Result<'b, 'e> = Result.bind f result


namespace FSharp.Prelude

open FSharp.Prelude.Operators.Result

[<RequireQualifiedAccess>]
module Result =

    /// Wraps a value in an Ok Result.
    let singleton (value: 'a) : Result<'a, 'e> = Ok value

    let apply (f: Result<'a -> 'b, 'e>) (result: Result<'a, 'e>) : Result<'b, 'e> = f <*> result

    let bindError (f: 'e1 -> Result<'a, 'e2>) (result: Result<'a, 'e1>) : Result<'a, 'e2> =
        match result with
        | Ok ok -> Ok ok
        | Error error -> f error

    let map2 (f: 'a -> 'b -> 'c) (result1: Result<'a, 'e>) (result2: Result<'b, 'e>) : Result<'c, 'e> =
        f <!> result1 <*> result2

    let andMap (result: Result<'a, 'e>) (f: Result<'a -> 'b, 'e>) : Result<'b, 'e> = map2 (|>) result f

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (result: Result<'a, 'e1>) : Result<'b, 'e2> =
        (Result.map f >> Result.mapError g) result

    let rec private traverser (f: 'a -> Result<'b, 'e>) folder state xs =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            folder head state
            |> function
            | Ok _ as this -> traverser f folder this tail
            | Error _ as this -> this

    let mapM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) results

    let sequence (results: Result<'a, 'e> list) : Result<'a list, 'e> = mapM id results

    let traverse (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        traverser f (fun head tail -> cons <!> f head <*> tail) (singleton []) results

    let sequenceA (results: Result<'a, 'e> list) : Result<'a list, 'e> = traverse id results

    let zip (result1: Result<'a, 'e>) (result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
        (fun a b -> a, b) <!> result1 <*> result2

    let ofOption (error: 'e) (option: 'a option) : Result<'a, 'e> =
        match option with
        | Some thing -> Ok thing
        | None -> Error error

    let ofChoice (choice: Choice<'a, 'e>) : Result<'a, 'e> =
        match choice with
        | Choice1Of2 left -> Ok left
        | Choice2Of2 right -> Error right

    /// Creates a safe version of the supplied function, returning Error(exn) instead of throwing an exception.
    let ofThrowable (f: 'a -> 'b) (a: 'a) : Result<'b, exn> =
        try
            Ok(f a)
        with exn -> Error exn

[<AutoOpen>]
module ResultCE =

    type ResultBuilder() =
        member this.Return(value: 'a) : Result<'a, 'e> = Result.singleton value

        member this.ReturnFrom(result: Result<'a, 'e>) : Result<'a, 'e> = result

        member this.Zero() : Result<unit, 'e> = Result.singleton ()

        member this.Bind(result: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) : Result<'b, 'e> = Result.bind f result

        member this.Delay(f: unit -> Result<'a, 'e>) : unit -> Result<'a, 'e> = f

        member this.Run(f: unit -> Result<'a, 'e>) : Result<'a, 'e> = f ()

        member this.Combine(result: Result<unit, 'e>, f: unit -> Result<'a, 'e>) : Result<'a, 'e> = Result.bind f result

        member this.TryWith(f: unit -> Result<'a, 'e>, g: exn -> Result<'a, 'e>) : Result<'a, 'e> =
            try
                this.Run f
            with exn -> g exn

        member this.TryFinally(f: unit -> Result<'a, 'e>, g: unit -> unit) : Result<'a, 'e> =
            try
                this.Run f
            finally
                g ()

        member this.Using(disposable: 'a :> System.IDisposable, f: 'a -> Result<'a, 'e>) : Result<'a, 'e> =
            this.TryFinally(
                (fun () -> f disposable),
                (fun () ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose())
            )

        member this.While(f: unit -> bool, g: unit -> Result<unit, 'e>) : Result<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                this.Run g
                |> Result.bind (fun () -> this.While(f, g))

        member this.BindReturn(result: Result<'a, 'e>, f: 'a -> 'b) : Result<'b, 'e> = Result.map f result

        member this.MergeSources(result1: Result<'a, 'e>, result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
            Result.zip result1 result2

    let result = ResultBuilder()
