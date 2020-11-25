namespace FSharp.Prelude

type AsyncResult<'a, 'b> = Async<Result<'a, 'b>>

module AsyncResult =
    let singleton (value: 'a): AsyncResult<'a, 'b> =
        (Result.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
        (Result.map >> Async.map) f asyncResult

    let bind (f: 'a -> AsyncResult<'b, 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
        Async.bind (function
            | Ok ok -> f ok
            | Error error -> Async.singleton (Error error)) asyncResult

    let bindError (f: 'a -> AsyncResult<'b, 'c>) (asyncResult: AsyncResult<'b, 'a>): AsyncResult<'b, 'c> =
        Async.bind (function
            | Ok ok -> singleton ok
            | Error error -> f error) asyncResult

    let apply (f: AsyncResult<('a -> 'b), 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
        async {
            let! runF = Async.StartChild f
            let! runAsyncResult = Async.StartChild asyncResult
            let! f' = runF
            let! result = runAsyncResult
            return Result.apply f' result
        }

    let andMap (asyncResult: AsyncResult<'a, 'b>) (f: AsyncResult<('a -> 'c), 'b>): AsyncResult<'c, 'b> =
        apply f asyncResult

    [<AutoOpen>]
    module AsyncResultOperators =
        let (<!>) (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> = map f asyncResult

        let (<*>) (f: AsyncResult<('a -> 'b), 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
            apply f asyncResult

        let (>>=) (f: 'a -> AsyncResult<'b, 'c>) (asyncResult: AsyncResult<'a, 'c>): AsyncResult<'b, 'c> =
            bind f asyncResult

    let map2 (f: 'a -> 'b -> 'c)
             (asyncResult1: AsyncResult<'a, 'd>)
             (asyncResult2: AsyncResult<'b, 'd>)
             : AsyncResult<'c, 'd> =
        f <!> asyncResult1 <*> asyncResult2

    let sequence asyncResults =
        List.foldr (fun asyncResult1 asyncResult2 -> List.cons <!> asyncResult1 <*> asyncResult2) (singleton [])
            asyncResults

    let zip (asyncResult1: AsyncResult<'a, 'b>) (asyncResult2: AsyncResult<'c, 'b>): AsyncResult<('a * 'c), 'b> =
        (fun a b -> a, b)
        <!> asyncResult1
        <*> asyncResult2

    let ofAsync (asyncOp: Async<'a>): AsyncResult<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofResult (result: 'a): Async<'a> = Async.singleton result

    let ofOption error option = ofResult (Result.ofOption error option)

    let ofTask (lazyTask: unit -> System.Threading.Tasks.Task<'a>): AsyncResult<'a, exn> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Result.ofChoice

    let ofUnitTask (lazyUnitTask: unit -> System.Threading.Tasks.Task): AsyncResult<unit, exn> =
        async.Delay(lazyUnitTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Result.ofChoice

    [<AutoOpen>]
    module AsyncResultCE =
        type AsyncResultBuilder() =
            member _.Return(x) = singleton x

            member _.ReturnFrom(m: AsyncResult<_, _>) = m

            member _.Zero() = singleton ()

            member _.Bind(m, f) = f >>= m

            member _.Bind((_, error), f) = bindError f error

            member _.Delay(f: unit -> AsyncResult<_, _>) = async.Delay f

            member _.Combine(m1: AsyncResult<unit, 'b>, m2: AsyncResult<'a, 'b>) = (fun () -> m2) >>= m1

            member _.TryWith(m: AsyncResult<'a, 'b>, f: exn -> AsyncResult<'a, 'b>) = async.TryWith(m, f)

            member _.TryFinally(m: AsyncResult<'a, 'b>, f: unit -> unit) = async.TryFinally(m, f)

            member _.Using(m: 'a :> System.IDisposable, f: 'a -> AsyncResult<'a, 'b>) = async.Using(m, f)

            member _.BindReturn(m, f) = f <!> m

            member _.MergeSources(m1, m2) = zip m1 m2

        let asyncResult = AsyncResultBuilder()
