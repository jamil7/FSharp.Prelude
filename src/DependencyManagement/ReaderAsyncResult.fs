namespace FSharp.Prelude.Operators.ReaderResult

[<AutoOpen>]
module ResultReader =

    open FSharp.Prelude

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (rar: 'r -> Async<Result<'a, 'err>>) : 'r -> Async<Result<'b, 'err>> =
        fun r -> AsyncResult.map f (rar r)

    /// Infix apply operator.
    let inline (<*>)
        (f: 'r -> Async<Result<'a -> 'b, 'e>>)
        (rar: 'r -> Async<Result<'a, 'e>>)
        : 'r -> Async<Result<'b, 'e>> =
        fun e -> AsyncResult.apply (f e) (rar e)

    /// Infix bind operator.
    let inline (>>=)
        (rar: 'r -> Async<Result<'a, 'e>>)
        (f: 'a -> 'r -> Async<Result<'b, 'e>>)
        : 'r -> Async<Result<'b, 'e>> =
        fun e -> AsyncResult.bind (fun a -> f a e) (rar e)

namespace FSharp.Prelude

open System.Threading.Tasks

type ReaderAsyncResult<'env, 'a, 'err> = 'env -> AsyncResult<'a, 'err>

[<RequireQualifiedAccess>]
module ReaderAsyncResult =

    open FSharp.Prelude.Operators.ReaderResult

    let singleton (x: 'a) : ReaderAsyncResult<'r, 'a, 'e> = fun _ -> AsyncResult.singleton x

    let map (f: 'a -> 'b) (rar: ReaderAsyncResult<'r, 'a, 'e>) : ReaderAsyncResult<'r, 'b, 'e> = f <!> rar

    let apply
        (f: ReaderAsyncResult<'r, 'a -> 'b, 'e>)
        (rar: ReaderAsyncResult<'r, 'a, 'e>)
        : ReaderAsyncResult<'r, 'b, 'e> =
        f <*> rar

    let bind
        (f: 'a -> ReaderAsyncResult<'r, 'b, 'e>)
        (rar: ReaderAsyncResult<'r, 'a, 'e>)
        : ReaderAsyncResult<'r, 'b, 'e> =
        rar >>= f

    let mapError (f: 'e1 -> 'e2) (rar: ReaderAsyncResult<'r, 'a, 'e1>) : ReaderAsyncResult<'r, 'a, 'e2> =
        fun e -> AsyncResult.mapError f (rar e)

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (rar: ReaderAsyncResult<'r, 'a, 'e1>) : ReaderAsyncResult<'r, 'b, 'e2> =
        (map f >> mapError g) rar

    let run (environment: 'r) (rar: ReaderAsyncResult<'r, 'a, 'e>) : Result<'a, 'e> =
        rar environment |> Async.RunSynchronously

    let ask<'r, 'a, 'e> : ReaderAsyncResult<'r, 'r, 'e> = AsyncResult.singleton >> id

    let asks (f: 'r -> 'a) : ReaderAsyncResult<'r, 'a, 'e> = f <!> ask

    let withReader (f: 'r1 -> 'r2) (rar: ReaderAsyncResult<'r2, 'a, 'e>) : ReaderAsyncResult<'r1, 'a, 'e> = f >> rar

    let ofAsync (asyncOp: Async<'a>) : ReaderAsyncResult<'r, 'a, exn> = fun _ -> AsyncResult.ofAsync asyncOp

    let ofOption (error: 'e) (option: 'a option) : ReaderAsyncResult<'r, 'a, 'e> =
        fun _ -> AsyncResult.ofOption error option

    let ofResult (result: Result<'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> = fun _ -> AsyncResult.ofResult result

    let ofChoice (choice: Choice<'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> = fun _ -> AsyncResult.ofChoice choice

    let ofTask (lazyTask: unit -> Task<'a>) : ReaderAsyncResult<'r, 'a, exn> = fun _ -> AsyncResult.ofTask lazyTask

    let ofUnitTask (lazyUnitTask: unit -> Task) : ReaderAsyncResult<'r, unit, exn> =
        fun _ -> AsyncResult.ofUnitTask lazyUnitTask

[<AutoOpen>]
module ReaderAsyncResultCE =
    type ReaderAsyncResultBuilder<'r, 'e>() =
        member _.Return(a: 'a) : ReaderAsyncResult<'r, 'a, 'e> = ReaderAsyncResult.singleton a

        member _.ReturnFrom(a: ReaderAsyncResult<'r, 'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> = a

        member _.Zero() : ReaderAsyncResult<'r, unit, 'e> = ReaderAsyncResult.singleton ()

        member _.Bind(reader: ReaderAsyncResult<'r, 'a, 'e>, f: 'a -> ReaderAsyncResult<'r, 'a, 'e>) =
            ReaderAsyncResult.bind f reader

        member _.Delay(f: unit -> ReaderAsyncResult<'r, 'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> =
            ReaderAsyncResult.bind f (ReaderAsyncResult.singleton ())

        member _.Combine
            (
                unitReader: ReaderAsyncResult<'r, unit, 'e>,
                reader: ReaderAsyncResult<'r, 'a, 'e>
            ) : ReaderAsyncResult<'r, 'a, 'e> =
            ReaderAsyncResult.bind (fun () -> reader) unitReader

        member _.TryWith
            (
                reader: ReaderAsyncResult<'r, 'a, 'e>,
                f: exn -> ReaderAsyncResult<'r, 'a, 'e>
            ) : ReaderAsyncResult<'r, 'a, 'e> =
            fun r ->
                try
                    reader r
                with e -> (f e) r

        member this.TryFinally(reader: ReaderAsyncResult<'r, 'a, 'e>, f: unit -> unit) : ReaderAsyncResult<'r, 'a, 'e> =
            fun r ->
                try
                    reader r
                finally
                    f ()

        member this.Using
            (
                disposable: 'a :> #System.IDisposable,
                f: 'a -> ReaderAsyncResult<'r, 'b, 'e>
            ) : ReaderAsyncResult<'r, 'b, 'e> =
            this.TryFinally(
                f disposable,
                fun () ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose()
            )

        member this.BindReturn(reader: ReaderAsyncResult<'r, 'a, 'e>, f: 'a -> 'b) : ReaderAsyncResult<'r, 'b, 'e> =
            ReaderAsyncResult.map f reader

        member this.While(f: unit -> bool, reader: ReaderAsyncResult<'r, unit, 'e>) : ReaderAsyncResult<'r, unit, 'e> =
            if not (f ()) then
                ReaderAsyncResult.singleton ()
            else
                reader
                |> ReaderAsyncResult.bind (fun () -> this.While(f, reader))

        member this.For(sequence: #seq<'a>, f: 'a -> ReaderAsyncResult<'r, unit, 'e>) =
            this.Using(
                sequence.GetEnumerator(),
                fun enumerator -> this.While(enumerator.MoveNext, this.Delay(fun () -> f enumerator.Current))
            )

        member inline this.Source(rar: ReaderAsyncResult<'r, 'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> = rar

    let readerAsyncResult<'r, 'e> = ReaderAsyncResultBuilder<'r, 'e>()

[<AutoOpen>]
module ReaderAsyncResultCEExtensions =
    type ReaderAsyncResultBuilder<'r, 'e> with
        member inline this.Source(sequence: #seq<'a>) : #seq<'a> = sequence

        member inline this.Source(asyncOp: Async<'a>) : ReaderAsyncResult<'e, 'a, exn> =
            ReaderAsyncResult.ofAsync asyncOp

        member inline this.Source(result: Result<'a, 'e>) : ReaderAsyncResult<'r, 'a, 'e> =
            ReaderAsyncResult.ofResult result

        member inline this.Source(task: Task<'a>) : ReaderAsyncResult<'r, 'a, exn> =
            ReaderAsyncResult.ofTask (fun () -> task)

        member inline this.Source(unitTask: Task) : ReaderAsyncResult<'r, unit, exn> =
            ReaderAsyncResult.ofUnitTask (fun () -> unitTask)
