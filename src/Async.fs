namespace FSharp.Prelude.Operators.Async

[<AutoOpen>]
module AsyncOperators =
    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncOp: Async<'a>): Async<'b> = async.Bind(asyncOp, f >> async.Return)

    /// Infix apply operator.
    let inline (<*>) (f: Async<('a -> 'b)>) (asyncOp: Async<'a>): Async<'b> =
        async {
            let! f' = f
            let! asyncOp' = asyncOp
            return f' asyncOp'
        }

    /// Infix parallel apply operator.
    let inline (<&>) (f: Async<('a -> 'b)>) (asyncOp: Async<'a>): Async<'b> =
        async {
            let! runF = Async.StartChildAsTask f
            let! runAsyncOp = Async.StartChildAsTask asyncOp
            let! f' = Async.AwaitTask runF
            let! asyncOpRes = Async.AwaitTask runAsyncOp
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

    let applyParallel (f: Async<('a -> 'b)>) (asyncOp: Async<'a>): Async<'b> = f <&> asyncOp

    let bind (f: 'a -> Async<'b>) (asyncOp: Async<'a>): Async<'b> = f >>= asyncOp

    let map2 (f: 'a -> 'b -> 'c) (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'c> = f <!> asyncOp1 <*> asyncOp2

    let andMap (asyncOp: Async<'a>) (f: Async<('a -> 'b)>): Async<'b> = map2 (|>) asyncOp f

    let private traverser (f: Async<('b list -> 'b list)> -> Async<'b list> -> Async<'b list>)
                          (g: 'a -> Async<'b>)
                          (list: 'a list)
                          : Async<'b list> =
        List.foldBack (fun head tail ->
            (fun head' tail' -> head' :: tail') <!> (g head)
            |> f
            <| tail) list (singleton [])

    let traverse (f: 'a -> Async<'b>) (list: 'a list): Async<'b list> = traverser (<*>) f list

    let traverseParallel (f: 'a -> Async<'b>) (list: 'a list): Async<'b list> = traverser (<&>) f list

    let sequence (asyncOps: Async<'a> list): Async<'a list> = traverse id asyncOps

    let parallel' (asyncOps: Async<'a> list): Async<'a list> =
        async {
            let! resArray = Async.Parallel asyncOps
            return List.ofArray resArray
        }

    let private zipper f (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'a * 'b> =
        (fun a b -> a, b) <!> asyncOp1 |> f <| asyncOp2

    let zip (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'a * 'b> = zipper (<*>) asyncOp1 asyncOp2

    let zipParallel (asyncOp1: Async<'a>) (asyncOp2: Async<'b>): Async<'a * 'b> = zipper (<&>) asyncOp1 asyncOp2

[<AutoOpen>]
module AsyncExtension =
    type Async with
        /// A replacement for Async.AwaitTask that throws inner exceptions if they exist.
        static member AwaitTaskWithInnerException(task: Task<'T>): Async<'T> =
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
        static member AwaitTaskWithInnerException(task: Task): Async<unit> =
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

[<AutoOpen>]
module AsyncCEExtensions =
    type FSharp.Control.AsyncBuilder with
        member _.Bind(task: Task<'a>, f: 'a -> Async<'b>): Async<'b> =
            Async.bind f (Async.AwaitTaskWithInnerException task)

        member _.Bind(unitTask: Task, f: unit -> Async<unit>): Async<unit> =
            Async.bind f (Async.AwaitTaskWithInnerException unitTask)

        member _.BindReturn(async: Async<'a>, f: 'a -> 'b): Async<'b> = Async.map f async

        member _.MergeSources(async1: Async<'a>, async2: Async<'b>): Async<'a * 'b> = Async.zipParallel async1 async2
