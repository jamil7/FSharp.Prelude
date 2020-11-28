namespace FSharp.Prelude.Operators.AsyncResult

open FSharp.Prelude

[<AutoOpen>]
module AsyncResultOperators =
    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncResult: Async<Result<'a, 'c>>): Async<Result<'b, 'c>> =
        (Result.map >> Async.map) f asyncResult

    /// Infix apply operator.
    let inline (<*>) (f: Async<Result<('a -> 'b), 'c>>) (asyncResult: Async<Result<'a, 'c>>): Async<Result<'b, 'c>> =
        async {
            let! runF = Async.StartChild f
            let! runAsyncResult = Async.StartChild asyncResult
            let! f' = runF
            let! result = runAsyncResult
            return Result.apply f' result
        }

    /// Infix bind operator.
    let inline (>>=) (f: 'a -> Async<Result<'b, 'c>>) (asyncResult: Async<Result<'a, 'c>>): Async<Result<'b, 'c>> =
        Async.bind (function
            | Ok ok -> f ok
            | Error error -> Async.singleton (Error error)) asyncResult

namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncResult
open System.Threading.Tasks

type AsyncResult<'a, 'b> = Async<Result<'a, 'b>>

[<RequireQualifiedAccess>]
module AsyncResult =
    /// Wraps a value in an AsyncResult.
    let singleton (value: 'a): AsyncResult<'a, 'b> =
        (Result.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> = f <!> asyncResult

    let apply (f: AsyncResult<('a -> 'b), 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
        f <*> asyncResult

    let bind (f: 'a -> AsyncResult<'b, 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> = f >>= asyncResult

    let bindError (f: 'a -> AsyncResult<'b, 'c>) (asyncResult: AsyncResult<'b, 'a>): AsyncResult<'b, 'c> =
        Async.bind (function
            | Ok ok -> singleton ok
            | Error error -> f error) asyncResult

    let map2 (f: 'a -> 'b -> 'c)
             (asyncResult1: AsyncResult<'a, 'd>)
             (asyncResult2: AsyncResult<'b, 'd>)
             : AsyncResult<'c, 'd> =
        f <!> asyncResult1 <*> asyncResult2

    let andMap (asyncResult: AsyncResult<'a, 'b>) (f: AsyncResult<('a -> 'c), 'b>): AsyncResult<'c, 'b> =
        map2 (|>) asyncResult f

    let sequence (asyncResults: AsyncResult<'a, 'b> list): AsyncResult<'a list, 'b> =
        List.foldBack (fun asyncResult1 asyncResult2 ->
            (fun head tail -> head :: tail)
            <!> asyncResult1
            <*> asyncResult2) asyncResults (singleton [])

    let zip (asyncResult1: AsyncResult<'a, 'b>) (asyncResult2: AsyncResult<'c, 'b>): AsyncResult<('a * 'c), 'b> =
        (fun a b -> a, b)
        <!> asyncResult1
        <*> asyncResult2

    let ofAsync (asyncOp: Async<'a>): AsyncResult<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofOption error option =
        Async.singleton (Result.ofOption error option)

    let ofResult (result: 'a): Async<'a> = Async.singleton result

    let ofTask (lazyTask: unit -> Task<'a>): AsyncResult<'a, exn> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofUnitTask (lazyUnitTask: unit -> Task): AsyncResult<unit, exn> =
        async.Delay(lazyUnitTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Result.ofChoice

[<AutoOpen>]
module AsyncResultCE =
    type AsyncResultBuilder() =
        member _.Return(x) = AsyncResult.singleton x

        member _.ReturnFrom(m: AsyncResult<_, _>) = m

        member _.Zero() = AsyncResult.singleton ()

        member _.Bind(m, f) = f >>= m

        member _.Bind((_, error), f) = AsyncResult.bindError f error

        member _.Delay(f: unit -> AsyncResult<_, _>) = async.Delay f

        member _.Combine(m1: AsyncResult<unit, 'b>, m2: AsyncResult<'a, 'b>) = (fun () -> m2) >>= m1

        member _.TryWith(m: AsyncResult<'a, 'b>, f: exn -> AsyncResult<'a, 'b>) = async.TryWith(m, f)

        member _.TryFinally(m: AsyncResult<_, _>, f: unit -> unit) = async.TryFinally(m, f)

        member _.Using(m: 'a :> System.IDisposable, f: 'a -> AsyncResult<'a, 'b>) = async.Using(m, f)

        member _.BindReturn(m, f) = f <!> m

        member _.MergeSources(m1, m2) = AsyncResult.zip m1 m2

    let asyncResult = AsyncResultBuilder()
