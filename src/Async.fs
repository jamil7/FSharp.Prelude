namespace FSharp.Prelude.Operators.Async

[<AutoOpen>]
module AsyncOperators =
    let (!>) (value: 'a) : Async<'a> = async.Return value

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (asyncOp: Async<'a>) : Async<'b> = async.Bind(asyncOp, f >> async.Return)

    /// Infix apply operator.
    let inline (<*>) (f: Async<'a -> 'b>) (asyncOp: Async<'a>) : Async<'b> =
        async {
            let! f' = f
            let! asyncOp' = asyncOp
            return f' asyncOp'
        }

    /// Infix parallel apply operator.
    let inline (<&>) (f: Async<'a -> 'b>) (asyncOp: Async<'a>) : Async<'b> =
        async {
            let! runF = Async.StartChildAsTask f
            let! runAsyncOp = Async.StartChildAsTask asyncOp
            let! f' = Async.AwaitTask runF
            let! asyncOpRes = Async.AwaitTask runAsyncOp
            return f' asyncOpRes
        }

    /// Infix bind operator.
    let inline (>>=) (asyncOp: Async<'a>) (f: 'a -> Async<'b>) : Async<'b> = async.Bind(asyncOp, f)


namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    open FSharp.Prelude.Operators.Async

    let traverseAsyncM (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun head' -> tail >>= (fun tail' -> !>(List.cons head' tail'))))
            asyncOps
            (!> [])

    let traverseAsyncA (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack (fun head tail -> List.cons <!> f head <*> tail) asyncOps (!> [])

    let traverseAsyncAParallel (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack (fun head tail -> List.cons <!> f head <&> tail) asyncOps (!> [])

    let sequenceAsyncM (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncM id asyncOps

    let sequenceAsyncA (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncA id asyncOps

    let sequenceAsyncAParallel (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncAParallel id asyncOps


[<RequireQualifiedAccess>]
module Async =

    open FSharp.Prelude.Operators.Async

    /// Wraps a value in an Async.
    let singleton (value: 'a) : Async<'a> = !>value

    let map (f: 'a -> 'b) (asyncOp: Async<'a>) : Async<'b> = f <!> asyncOp

    let apply (f: Async<'a -> 'b>) (asyncOp: Async<'a>) : Async<'b> = f <*> asyncOp

    let applyParallel (f: Async<'a -> 'b>) (asyncOp: Async<'a>) : Async<'b> = f <&> asyncOp

    let bind (f: 'a -> Async<'b>) (asyncOp: Async<'a>) : Async<'b> = asyncOp >>= f

    let map2 (f: 'a -> 'b -> 'c) (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) : Async<'c> = f <!> asyncOp1 <*> asyncOp2

    let andMap (asyncOp: Async<'a>) (f: Async<'a -> 'b>) : Async<'b> = map2 (|>) asyncOp f

    let sequence (asyncOps: Async<'a> list) : Async<'a list> = List.sequenceAsyncM asyncOps

    let parallel' (asyncOps: Async<'a> list) : Async<'a list> =
        async {
            let! resArray = Async.Parallel asyncOps
            return List.ofArray resArray
        }

    let zip (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) : Async<'a * 'b> =
        (fun a b -> a, b) <!> asyncOp1 <*> asyncOp2

    let zipParallel (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) : Async<'a * 'b> =
        (fun a b -> a, b) <!> asyncOp1 <&> asyncOp2


[<AutoOpen>]
module AsyncExtension =

    open System.Threading.Tasks

    type Async with
        /// A replacement for Async.AwaitTask that throws inner exceptions if they exist.
        static member AwaitTaskWithInnerException(task: Task<'T>) : Async<'T> =
            Async.FromContinuations
                (fun (success, exception', _cancellationToken) ->
                    task.ContinueWith
                        (fun (t: Task<'T>) ->
                            if t.IsFaulted then
                                if t.Exception.InnerExceptions.Count = 1 then
                                    exception' t.Exception.InnerExceptions.[0]
                                else
                                    exception' t.Exception
                            elif t.IsCanceled then
                                exception' (TaskCanceledException())
                            else
                                success t.Result)
                    |> ignore)

        /// A replacement for Async.AwaitTask that throws inner exceptions if they exist.
        static member AwaitTaskWithInnerException(task: Task) : Async<unit> =
            Async.FromContinuations
                (fun (success, exception', _cancellationToken) ->
                    task.ContinueWith
                        (fun (t: Task) ->
                            if t.IsFaulted then
                                if t.Exception.InnerExceptions.Count = 1 then
                                    exception' t.Exception.InnerExceptions.[0]
                                else
                                    exception' t.Exception
                            elif t.IsCanceled then
                                exception' (TaskCanceledException())
                            else
                                success ())
                    |> ignore)


[<AutoOpen>]
module AsyncCEExtensions =

    open System.Threading.Tasks

    type FSharp.Control.AsyncBuilder with
        member _.Bind(task: Task<'a>, f: 'a -> Async<'b>) : Async<'b> =
            Async.bind f (Async.AwaitTaskWithInnerException task)

        member _.Bind(actionTask: Task, f: unit -> Async<unit>) : Async<unit> =
            Async.bind f (Async.AwaitTaskWithInnerException actionTask)

        member _.BindReturn(async: Async<'a>, f: 'a -> 'b) : Async<'b> = Async.map f async

        member _.MergeSources(async1: Async<'a>, async2: Async<'b>) : Async<'a * 'b> = Async.zipParallel async1 async2
