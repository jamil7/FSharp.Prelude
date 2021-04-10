namespace FSharp.Prelude.Operators.Result

[<AutoOpen>]
module ResultOperators =
    let (!>) (value: 'a) : Result<'a, 'e> = Ok value

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

    let (>=>) (f: 'a -> Result<'b, 'e>) (g: 'b -> Result<'c, 'e>) : 'a -> Result<'c, 'e> =
        fun x ->
            match f x with
            | Ok ok -> g ok
            | Error e -> Error e


namespace FSharp.Prelude

open FSharp.Prelude.Operators.Result

[<RequireQualifiedAccess>]
module List =

    let traverseResultM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun head' -> tail >>= (fun tail' -> !>(List.cons head' tail'))))
            results
            (!> [])

    let traverseResultA (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        List.foldBack (fun head tail -> List.cons <!> f head <*> tail) results (!> [])

    let sequenceResultM (asyncOps: Result<'a, 'e> list) : Result<'a list, 'e> = traverseResultM id asyncOps

    let sequenceResultA (results: Result<'a, 'e> list) : Result<'a list, 'e> = traverseResultA id results


[<RequireQualifiedAccess>]
module Result =

    /// Wraps a value in an Ok Result.
    let singleton (value: 'a) : Result<'a, 'e> = !>value

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

    let compose (f: 'a -> Result<'b, 'e>) (g: 'b -> Result<'c, 'e>) : 'a -> Result<'c, 'e> = f >=> g

    let traverse (f: 'a -> Result<'b, 'e>) (list: 'a list) : Result<'b list, 'e> =
        List.foldBack
            (fun head tail ->
                (fun head' tail' -> head' :: tail') <!> (f head)
                <*> tail)
            list
            (singleton [])

    let sequence (results: Result<'a, 'e> list) : Result<'a list, 'e> = traverse id results

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


[<AutoOpen>]
module ResultCE =

    type ResultBuilder() =
        member _.Return(value: 'a) : Result<'a, 'e> = Result.singleton value

        member _.ReturnFrom(result: Result<'a, 'e>) : Result<'a, 'e> = result

        member _.Zero() : Result<unit, 'e> = Result.singleton ()

        member _.Bind(result: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) : Result<'b, 'e> = Result.bind f result

        member _.Delay(f: unit -> Result<'a, 'e>) : unit -> Result<'a, 'e> = f

        member _.Run(f: unit -> Result<'a, 'e>) : Result<'a, 'e> = f ()

        member _.Combine(result: Result<unit, 'e>, f: unit -> Result<'a, 'e>) : Result<'a, 'e> = Result.bind f result

        member _.TryWith(f: unit -> Result<'a, 'e>, g: exn -> Result<'a, 'e>) : Result<'a, 'e> =
            try
                f ()
            with exn -> g exn

        member _.TryFinally(f: unit -> Result<'a, 'e>, g: unit -> unit) : Result<'a, 'e> =
            try
                f ()
            finally
                g ()

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> Result<'a, 'e>) : Result<'a, 'e> =
            try
                (fun () -> f disposable) ()
            finally
                (fun () ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose())
                    ()

        member this.While(f: unit -> bool, g: unit -> Result<unit, 'e>) : Result<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                g () |> Result.bind (fun () -> this.While(f, g))

        member _.BindReturn(result: Result<'a, 'e>, f: 'a -> 'b) : Result<'b, 'e> = Result.map f result

        member _.MergeSources(result1: Result<'a, 'e>, result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
            Result.zip result1 result2

    let result = ResultBuilder()
