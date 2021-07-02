namespace FSharp.Prelude.Operators.ReaderResult

[<AutoOpen>]
module ResultReader =

    open FSharp.Prelude

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (rr: 'r -> Result<'a, 'err>) : 'r -> Result<'b, 'err> = fun r -> Result.map f (rr r)

    /// Infix apply operator.
    let inline (<*>) (f: 'r -> Result<'a -> 'b, 'e>) (rr: 'r -> Result<'a, 'e>) : 'r -> Result<'b, 'e> =
        fun e -> Result.apply (f e) (rr e)

    /// Infix bind operator.
    let inline (>>=) (rr: 'r -> Result<'a, 'e>) (f: 'a -> 'r -> Result<'b, 'e>) : 'r -> Result<'b, 'e> =
        fun e -> Result.bind (fun a -> f a e) (rr e)

namespace FSharp.Prelude

type ReaderResult<'r, 'a, 'e> = 'r -> Result<'a, 'e>

[<RequireQualifiedAccess>]
module ReaderResult =

    open FSharp.Prelude.Operators.ReaderResult

    let singleton (x: 'a) : ReaderResult<'r, 'a, 'e> = fun _ -> Result.singleton x

    let map (f: 'a -> 'b) (rr: ReaderResult<'r, 'a, 'e>) : ReaderResult<'r, 'b, 'e> = f <!> rr

    let apply (f: ReaderResult<'r, 'a -> 'b, 'e>) (rr: ReaderResult<'r, 'a, 'e>) : ReaderResult<'r, 'b, 'e> = f <*> rr

    let bind (f: 'a -> ReaderResult<'r, 'b, 'e>) (rr: ReaderResult<'r, 'a, 'e>) : ReaderResult<'r, 'b, 'e> = rr >>= f

    let mapError (f: 'e1 -> 'e2) (rr: ReaderResult<'r, 'a, 'e1>) : ReaderResult<'r, 'a, 'e2> =
        fun e -> Result.mapError f (rr e)

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (rr: ReaderResult<'r, 'a, 'e1>) : ReaderResult<'r, 'b, 'e2> =
        (map f >> mapError g) rr

    let run (environment: 'r) (rr: ReaderResult<'r, 'a, 'e>) : Result<'a, 'e> = rr environment

    let ask<'r, 'a, 'e> : ReaderResult<'r, 'r, 'e> = Result.singleton >> id

    let asks (f: 'r -> 'a) : ReaderResult<'r, 'a, 'e> = f <!> ask

    let withReader (f: 'r1 -> 'r2) (reader: ReaderResult<'r2, 'a, 'e>) : ReaderResult<'r1, 'a, 'e> = f >> reader

    let ofResult (result: Result<'a, 'e>) : ReaderResult<'r, 'a, 'e> = fun _ -> result

    let ofOption (error: 'e) (option: 'a option) : ReaderResult<'r, 'a, 'e> = fun _ -> Result.ofOption error option

    let ofChoice (choice: Choice<'a, 'e>) : ReaderResult<'r, 'a, 'e> = fun _ -> Result.ofChoice choice

    let ofThrowable (f: 'a -> 'b) (a: 'a) : ReaderResult<'r, 'b, exn> = fun _ -> Result.ofThrowable f a


[<AutoOpen>]
module ReaderResultCE =
    type ReaderResultBuilder<'r, 'e>() =
        member _.Return(a: 'a) : ReaderResult<'r, 'a, 'e> = ReaderResult.singleton a

        member _.ReturnFrom(a: ReaderResult<'r, 'a, 'e>) : ReaderResult<'r, 'a, 'e> = a

        member _.Zero() : ReaderResult<'r, unit, 'e> = ReaderResult.singleton ()

        member _.Bind(reader: ReaderResult<'r, 'a, 'e>, f: 'a -> ReaderResult<'r, 'a, 'e>) = ReaderResult.bind f reader

        member _.Delay(f: unit -> ReaderResult<'r, 'a, 'e>) : ReaderResult<'r, 'a, 'e> =
            ReaderResult.bind f (ReaderResult.singleton ())

        member _.Combine
            (
                unitReader: ReaderResult<'r, unit, 'e>,
                reader: ReaderResult<'r, 'a, 'e>
            ) : ReaderResult<'r, 'a, 'e> =
            ReaderResult.bind (fun () -> reader) unitReader

        member _.TryWith
            (
                reader: ReaderResult<'r, 'a, 'e>,
                f: exn -> ReaderResult<'r, 'a, 'e>
            ) : ReaderResult<'r, 'a, 'e> =
            fun r ->
                try
                    reader r
                with e -> (f e) r

        member this.TryFinally(reader: ReaderResult<'r, 'a, 'e>, f: unit -> unit) : ReaderResult<'r, 'a, 'e> =
            fun r ->
                try
                    reader r
                finally
                    f ()

        member this.Using
            (
                disposable: 'a :> #System.IDisposable,
                f: 'a -> ReaderResult<'r, 'b, 'e>
            ) : ReaderResult<'r, 'b, 'e> =
            this.TryFinally(
                f disposable,
                fun () ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose()
            )

        member this.BindReturn(reader: ReaderResult<'r, 'a, 'e>, f: 'a -> 'b) : ReaderResult<'r, 'b, 'e> =
            ReaderResult.map f reader

        member this.While(f: unit -> bool, reader: ReaderResult<'r, unit, 'e>) : ReaderResult<'r, unit, 'e> =
            if not (f ()) then
                ReaderResult.singleton ()
            else
                reader
                |> ReaderResult.bind (fun () -> this.While(f, reader))

        member this.For(sequence: #seq<'a>, f: 'a -> ReaderResult<'r, unit, 'e>) =
            this.Using(
                sequence.GetEnumerator(),
                fun enumerator -> this.While(enumerator.MoveNext, this.Delay(fun () -> f enumerator.Current))
            )

    let readerResult<'r, 'e> = ReaderResultBuilder<'r, 'e>()
