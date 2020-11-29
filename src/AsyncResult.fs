namespace FSharp.Prelude.Operators.AsyncResult

open FSharp.Prelude

[<AutoOpen>]
module AsyncResultOperators =
    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncResult: Async<Result<'a, 'e>>): Async<Result<'b, 'e>> =
        (Result.map >> Async.map) f asyncResult

    /// Infix apply operator.
    let inline (<*>) (f: Async<Result<('a -> 'b), 'e>>) (asyncResult: Async<Result<'a, 'e>>): Async<Result<'b, 'e>> =
        async {
            let! runF = Async.StartChild f
            let! runAsyncResult = Async.StartChild asyncResult
            let! f' = runF
            let! result = runAsyncResult
            return Result.apply f' result
        }

    /// Infix bind operator.
    let inline (>>=) (f: 'a -> Async<Result<'b, 'e>>) (asyncResult: Async<Result<'a, 'e>>): Async<Result<'b, 'e>> =
        Async.bind (function
            | Ok ok -> f ok
            | Error error -> Async.singleton (Error error)) asyncResult


namespace FSharp.Prelude

open FSharp.Prelude
open FSharp.Prelude.Operators.AsyncResult
open System.Threading.Tasks

type AsyncResult<'a, 'e> = Async<Result<'a, 'e>>

[<RequireQualifiedAccess>]
module AsyncResult =
    /// Wraps a value in an AsyncResult.
    let singleton (value: 'a): AsyncResult<'a, 'e> =
        (Result.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'e>): AsyncResult<'b, 'e> = f <!> asyncResult

    let apply (f: AsyncResult<('a -> 'b), 'e>) (asyncResult: AsyncResult<'a, 'e>): AsyncResult<'b, 'e> =
        f <*> asyncResult

    let bind (f: 'a -> AsyncResult<'b, 'e>) (asyncResult: AsyncResult<'a, 'e>): AsyncResult<'b, 'e> = f >>= asyncResult

    let bindError (f: 'e1 -> AsyncResult<'a, 'e2>) (asyncResult: AsyncResult<'a, 'e1>): AsyncResult<'a, 'e2> =
        Async.bind (function
            | Ok ok -> singleton ok
            | Error error -> f error) asyncResult

    let map2 (f: 'a -> 'b -> 'c)
             (asyncResult1: AsyncResult<'a, 'e>)
             (asyncResult2: AsyncResult<'b, 'e>)
             : AsyncResult<'c, 'e> =
        f <!> asyncResult1 <*> asyncResult2

    let andMap (asyncResult: AsyncResult<'a, 'e>) (f: AsyncResult<('a -> 'b), 'e>): AsyncResult<'b, 'e> =
        map2 (|>) asyncResult f

    let sequence (asyncResults: AsyncResult<'a, 'e> list): AsyncResult<'a list, 'e> =
        List.foldBack (fun asyncResult1 asyncResult2 ->
            (fun head tail -> head :: tail)
            <!> asyncResult1
            <*> asyncResult2) asyncResults (singleton [])

    let zip (asyncResult1: AsyncResult<'a, 'e>) (asyncResult2: AsyncResult<'b, 'e>): AsyncResult<'a * 'b, 'e> =
        (fun a b -> a, b)
        <!> asyncResult1
        <*> asyncResult2

    let ofAsync (asyncOp: Async<'a>): AsyncResult<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofOption (error: 'e) (option: 'a option): Async<Result<'a, 'e>> =
        Async.singleton (Result.ofOption error option)

    let ofResult (result: Result<'a, 'e>): AsyncResult<'a, 'e> = Async.singleton result

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
        member _.Return(value: 'a): AsyncResult<'a, 'e> = AsyncResult.singleton value

        member _.ReturnFrom(result: Result<'a, 'e>): AsyncResult<'a, 'e> = AsyncResult.ofResult result

        member _.ReturnFrom(asyncResult: AsyncResult<'a, 'e>): AsyncResult<'a, 'e> = asyncResult

        member _.Zero() = AsyncResult.singleton ()

        member _.Bind(result: Result<'a, 'e>, f: 'a -> Result<'b, 'e>): AsyncResult<'b, 'e> =
            AsyncResult.ofResult (Result.bind f result)

        member _.Bind(asyncResult: AsyncResult<'a, 'e>, f: 'a -> AsyncResult<'b, 'e>): AsyncResult<'b, 'e> =
            f >>= asyncResult

        member _.Bind(error: Result<'a, 'e1>, f: 'e1 -> Result<'a, 'e2>): AsyncResult<'a, 'e2> =
            AsyncResult.ofResult (Result.bindError f error)

        member _.Bind(error: AsyncResult<'a, 'e1>, f: 'e1 -> AsyncResult<'a, 'e2>): AsyncResult<'a, 'e2> =
            AsyncResult.bindError f error

        member _.Delay(f: unit -> AsyncResult<'a, 'e>): AsyncResult<'a, 'e> = async.Delay f

        member _.Combine(unitAsyncResult: AsyncResult<unit, 'e>, asyncResult: AsyncResult<'a, 'e>)
                         : AsyncResult<'a, 'e> =
            (fun () -> asyncResult) >>= unitAsyncResult

        member _.TryWith(asyncResult: AsyncResult<'a, 'e>, f: exn -> AsyncResult<'a, 'e>): AsyncResult<'a, 'e> =
            async.TryWith(asyncResult, f)

        member _.TryFinally(asyncResult: AsyncResult<'a, 'e>, f: unit -> unit): AsyncResult<'a, 'e> =
            async.TryFinally(asyncResult, f)

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> AsyncResult<'b, 'e>): AsyncResult<'b, 'e> =
            async.Using(disposable, f)

        member _.BindReturn(asyncResult: AsyncResult<'a, 'e>, f: 'a -> 'b): AsyncResult<'b, 'e> = f <!> asyncResult

        member _.MergeSources(asyncResult1: AsyncResult<'a, 'e>, asyncResult2: AsyncResult<'b, 'e>)
                              : AsyncResult<'a * 'b, 'e> =
            AsyncResult.zip asyncResult1 asyncResult2

    let asyncResult = AsyncResultBuilder()
