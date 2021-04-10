namespace FSharp.Prelude.Operators.AsyncResult

open FSharp.Prelude

[<AutoOpen>]
module AsyncResultOperators =
    let (!>) (value: 'a) : Async<Result<'a, 'e>> =
        (Result.singleton >> Async.singleton) value

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncResult: Async<Result<'a, 'e>>) : Async<Result<'b, 'e>> =
        (Result.map >> Async.map) f asyncResult

    /// Infix apply operator.
    let inline (<*>) (f: Async<Result<'a -> 'b, 'e>>) (asyncResult: Async<Result<'a, 'e>>) : Async<Result<'b, 'e>> =
        async {
            let! f' = f
            let! asyncResult' = asyncResult
            return Result.apply f' asyncResult'
        }

    /// Infix parallel apply operator.
    let inline (<&>) (f: Async<Result<'a -> 'b, 'e>>) (asyncResult: Async<Result<'a, 'e>>) : Async<Result<'b, 'e>> =
        async {
            let! f' = f
            and! asyncResult' = asyncResult
            return Result.apply f' asyncResult'
        }

    /// Infix bind operator.
    let inline (>>=) (asyncResult: Async<Result<'a, 'e>>) (f: 'a -> Async<Result<'b, 'e>>) : Async<Result<'b, 'e>> =
        Async.bind
            (function
            | Ok ok -> f ok
            | Error error -> Async.singleton (Error error))
            asyncResult

    let inline (>=>) (f: 'a -> Async<Result<'b, 'e>>) (g: 'b -> Async<Result<'c, 'e>>) : 'a -> Async<Result<'c, 'e>> =
        fun x ->
            async {
                let! f' = f x

                let! g' =
                    match f' with
                    | Ok ok -> g ok
                    | Error e -> Async.singleton (Error e)

                return g'
            }

namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncResult
open System.Threading.Tasks

type AsyncResult<'a, 'e> = Async<Result<'a, 'e>>

[<RequireQualifiedAccess>]
module List =
    let traverseAsyncResultM (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun head' -> tail >>= (fun tail' -> !>(List.cons head' tail'))))
            asyncResults
            (!> [])

    let traverseAsyncResultA (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        List.foldBack (fun head tail -> List.cons <!> f head <*> tail) asyncResults (!> [])

    let traverseAsyncResultAParallel (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        List.foldBack (fun head tail -> List.cons <!> f head <&> tail) asyncResults (!> [])

    let sequenceAsyncResultM (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        traverseAsyncResultM id asyncResults

    let sequenceAsyncResultA (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        traverseAsyncResultA id asyncResults

    let sequenceAsyncResultAParallel (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        traverseAsyncResultAParallel id asyncResults


[<RequireQualifiedAccess>]
module AsyncResult =

    /// Wraps a value in an AsyncResult.
    let singleton (value: 'a) : AsyncResult<'a, 'e> =
        (Result.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> = f <!> asyncResult

    let apply (f: AsyncResult<'a -> 'b, 'e>) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> =
        f <*> asyncResult

    let applyParallel (f: AsyncResult<'a -> 'b, 'e>) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> =
        f <&> asyncResult

    let bind (f: 'a -> AsyncResult<'b, 'e>) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> = asyncResult >>= f

    let mapError (f: 'e1 -> 'e2) (asyncResult: AsyncResult<'a, 'e1>) : AsyncResult<'a, 'e2> =
        Async.map (Result.mapError f) asyncResult

    let bindError (f: 'e1 -> AsyncResult<'a, 'e2>) (asyncResult: AsyncResult<'a, 'e1>) : AsyncResult<'a, 'e2> =
        Async.bind
            (function
            | Ok ok -> singleton ok
            | Error error -> f error)
            asyncResult

    let map2
        (f: 'a -> 'b -> 'c)
        (asyncResult1: AsyncResult<'a, 'e>)
        (asyncResult2: AsyncResult<'b, 'e>)
        : AsyncResult<'c, 'e> =
        f <!> asyncResult1 <*> asyncResult2

    let andMap (asyncResult: AsyncResult<'a, 'e>) (f: AsyncResult<'a -> 'b, 'e>) : AsyncResult<'b, 'e> =
        map2 (|>) asyncResult f

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (asyncResult: AsyncResult<'a, 'e1>) : AsyncResult<'b, 'e2> =
        (map f >> mapError g) asyncResult

    let compose (f: 'a -> AsyncResult<'b, 'e>) (g: 'b -> AsyncResult<'c, 'e>) : 'a -> AsyncResult<'c, 'e> = f >=> g

    let sequence (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        List.sequenceAsyncResultM asyncResults

    let parallel' (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        async {
            let! array = Async.Parallel asyncResults
            return Result.sequence (List.ofArray array)
        }

    let zip (asyncResult1: AsyncResult<'a, 'e>) (asyncResult2: AsyncResult<'b, 'e>) : AsyncResult<'a * 'b, 'e> =
        (fun a b -> a, b) <!> asyncResult1
        <*> asyncResult2

    let zipParallel (asyncResult1: AsyncResult<'a, 'e>) (asyncResult2: AsyncResult<'b, 'e>) : AsyncResult<'a * 'b, 'e> =
        (fun a b -> a, b) <!> asyncResult1
        <*> asyncResult2

    let ofAsync (asyncOp: Async<'a>) : AsyncResult<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofOption (error: 'e) (option: 'a option) : AsyncResult<'a, 'e> =
        Async.singleton (Result.ofOption error option)

    let ofResult (result: Result<'a, 'e>) : AsyncResult<'a, 'e> = Async.singleton result

    let ofTask (lazyTask: unit -> Task<'a>) : AsyncResult<'a, exn> =
        async.Delay(lazyTask >> Async.AwaitTaskWithInnerException)
        |> ofAsync

    let ofUnitTask (lazyUnitTask: unit -> Task) : AsyncResult<unit, exn> =
        async.Delay(lazyUnitTask >> Async.AwaitTaskWithInnerException)
        |> ofAsync

[<AutoOpen>]
module AsyncResultCE =
    type AsyncResultBuilder() =
        member _.Return(value: 'a) : AsyncResult<'a, 'e> = AsyncResult.singleton value

        member _.ReturnFrom(asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = asyncResult

        member _.Zero() : AsyncResult<unit, 'e> = AsyncResult.singleton ()

        member _.Bind(asyncResult: AsyncResult<'a, 'e>, f: 'a -> AsyncResult<'b, 'e>) : AsyncResult<'b, 'e> =
            AsyncResult.bind f asyncResult

        member _.Delay(f: unit -> AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = async.Delay f

        member _.Combine
            (
                unitAsyncResult: AsyncResult<unit, 'e>,
                asyncResult: AsyncResult<'a, 'e>
            ) : AsyncResult<'a, 'e> =
            AsyncResult.bind (fun () -> asyncResult) unitAsyncResult

        member _.TryWith(asyncResult: AsyncResult<'a, 'e>, f: exn -> AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> =
            async.TryWith(asyncResult, f)

        member _.TryFinally(asyncResult: AsyncResult<'a, 'e>, f: unit -> unit) : AsyncResult<'a, 'e> =
            async.TryFinally(asyncResult, f)

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> AsyncResult<'b, 'e>) : AsyncResult<'b, 'e> =
            async.Using(disposable, f)

        member _.BindReturn(asyncResult: AsyncResult<'a, 'e>, f: 'a -> 'b) : AsyncResult<'b, 'e> =
            AsyncResult.map f asyncResult

        member this.While(f: unit -> bool, asyncResult: AsyncResult<unit, 'e>) : AsyncResult<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                asyncResult
                |> AsyncResult.bind (fun () -> this.While(f, asyncResult))

        member _.MergeSources
            (
                asyncResult1: AsyncResult<'a, 'e>,
                asyncResult2: AsyncResult<'b, 'e>
            ) : AsyncResult<'a * 'b, 'e> =
            AsyncResult.zipParallel asyncResult1 asyncResult2

        member inline _.Source(asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = asyncResult

    let asyncResult = AsyncResultBuilder()

[<AutoOpen>]
module AsyncResultCEExtensions =
    type AsyncResultBuilder with
        member inline _.Source(asyncOp: Async<'a>) : AsyncResult<'a, exn> = AsyncResult.ofAsync asyncOp

        member inline _.Source(result: Result<'a, 'e>) : AsyncResult<'a, 'e> = AsyncResult.ofResult result

        member inline _.Source(task: Task<'a>) : AsyncResult<'a, exn> = AsyncResult.ofTask (fun () -> task)

        member inline _.Source(unitTask: Task) : AsyncResult<unit, exn> =
            AsyncResult.ofUnitTask (fun () -> unitTask)
