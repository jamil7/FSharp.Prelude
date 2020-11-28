namespace FSharp.Prelude.Operators.Async

[<AutoOpen>]
module AsyncOperators =
    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncOp: Async<'a>): Async<'b> = async.Bind(asyncOp, f >> async.Return)

    /// Infix apply operator.
    let inline (<*>) (f: Async<('a -> 'b)>) (asyncOp: Async<'a>): Async<'b> =
        async {
            let! runF = Async.StartChild f
            let! runAsyncOp = Async.StartChild asyncOp
            let! f' = runF
            let! asyncOpRes = runAsyncOp
            return f' asyncOpRes
        }

    /// Infix bind operator.
    let inline (>>=) (f: 'a -> Async<'b>) (asyncOp: Async<'a>): Async<'b> = async.Bind(asyncOp, f)

namespace FSharp.Prelude

open FSharp.Prelude.Operators.Async
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Async =
    /// Wraps a value in an Async.
    let singleton (value: 'a): Async<'a> = async.Return(value)

    let map (f: 'a -> 'b) (asyncOp: Async<'a>): Async<'b> = f <!> asyncOp

    let apply (f: Async<('a -> 'b)>) (asyncOp: Async<'a>): Async<'b> = f <*> asyncOp

    let bind (f: 'a -> Async<'b>) (asyncOp: Async<'a>): Async<'b> = f >>= asyncOp

    let map2 (f: 'a -> 'b -> 'c) (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'c> = f <!> asyncOp1 <*> asyncOp2

    let andMap (asyncOp: Async<'a>) (f: Async<('a -> 'b)>): Async<'b> = map2 (|>) asyncOp f

    let sequence (asyncOps: Async<'a> list): Async<'a list> =
        List.foldBack (fun asyncOp1 asyncOp2 ->
            (fun head tail -> head :: tail)
            <!> asyncOp1
            <*> asyncOp2) asyncOps (singleton [])

    let zip (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'a * 'b> =
        (fun a b -> a, b) <!> asyncOp1 <*> asyncOp2

    /// A replacement for Async.AwaitTask that throws inner exceptions if they exist.
    let awaitTaskWithInnerException (task: Task<'T>): Async<'T> =
        Async.FromContinuations(fun (success, exception', _cancellationToken) ->
            task.ContinueWith(fun (t: Task<'T>) ->
                if t.IsFaulted then
                    if t.Exception.InnerExceptions.Count = 1
                    then exception' t.Exception.InnerExceptions.[0]
                    else exception' t.Exception
                elif t.IsCanceled then
                    exception' (TaskCanceledException())
                else
                    success t.Result)
            |> ignore)

    /// A replacement for Async.AwaitTask that throws inner exceptions if they exist.
    let awaitUnitTaskWithInnerException (task: Task): Async<unit> =
        Async.FromContinuations(fun (success, exception', _cancellationToken) ->
            task.ContinueWith(fun (t: Task) ->
                if t.IsFaulted then
                    if t.Exception.InnerExceptions.Count = 1
                    then exception' t.Exception.InnerExceptions.[0]
                    else exception' t.Exception
                elif t.IsCanceled then
                    exception' (TaskCanceledException())
                else
                    success ())
            |> ignore)
