namespace FSharp.Prelude.Operators.Reader

[<AutoOpen>]
module ReaderOperators =

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (reader: 'r -> 'a) : 'r -> 'b = fun e -> f (reader e)

    /// Infix apply operator.
    let inline (<*>) (f: 'r -> 'a -> 'b) (reader: 'r -> 'a) : 'r -> 'b = fun e -> (f e) (reader e)

    /// Infix bind operator.
    let inline (>>=) (reader: 'r -> 'a) (f: 'a -> 'r -> 'b) : 'r -> 'b = fun e -> f (reader e) e

namespace FSharp.Prelude

type Reader<'e, 'a> = 'e -> 'a

[<RequireQualifiedAccess>]
module Reader =

    open FSharp.Prelude.Operators.Reader

    let singleton (x: 'a) : Reader<'r, 'a> = fun _ -> x

    let map (f: 'a -> 'b) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = f <!> reader

    let apply (f: Reader<'r, 'a -> 'b>) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = fun e -> (f e) (reader e)

    let bind (f: 'a -> Reader<'r, 'b>) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = fun e -> f (reader e) e

    let run (environment: 'r) (reader: Reader<'r, 'a>) : 'a = reader environment

    let ask : Reader<'e, 'e> = id

    let asks (f: 'r -> 'a) : Reader<'r, 'a> = map f ask

    let withReader (f: 'r1 -> 'r2) (reader: Reader<'r2, 'a>) : Reader<'r1, 'a> = f >> reader

[<AutoOpen>]
module ReaderCE =
    type ReaderBuilder<'r, 'e>() =
        member _.Return(a: 'a) : Reader<'r, 'a> = Reader.singleton a

        member _.ReturnFrom(a: Reader<'r, 'a>) : Reader<'r, 'a> = a

        member _.Zero() : Reader<'r, unit> = Reader.singleton ()

        member _.Bind(reader: Reader<'r, 'a>, f: 'a -> 'r -> 'a) = Reader.bind f reader

        member _.Delay(f: unit -> Reader<'r, 'a>) : Reader<'r, 'a> = Reader.bind f (Reader.singleton ())

        member _.Combine(unitAsyncResult: Reader<'r, unit>, asyncResult: Reader<'r, 'a>) : Reader<'r, 'a> =
            Reader.bind (fun () -> asyncResult) unitAsyncResult

        member _.TryWith(reader: Reader<'r, 'a>, f: exn -> Reader<'r, 'a>) : Reader<'r, 'a> =
            fun r ->
                try
                    reader r
                with e -> (f e) r

        member this.TryFinally(reader: Reader<'r, 'a>, f: unit -> unit) : Reader<'r, 'a> =
            fun r ->
                try
                    reader r
                finally
                    f ()

        member this.Using(disposable: 'a :> #System.IDisposable, f) : Reader<'r, 'a> =
            this.TryFinally(
                (fun _ -> f disposable),
                (fun _ ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose())
            )

        member this.BindReturn(asyncResult: AsyncResult<'a, 'e>, f: 'a -> 'b) : AsyncResult<'b, 'e> =
            AsyncResult.map f asyncResult

        member this.While(f: unit -> bool, asyncResult: AsyncResult<unit, 'e>) : AsyncResult<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                asyncResult
                |> AsyncResult.bind (fun () -> this.While(f, asyncResult))

        member this.MergeSources
            (
                asyncResult1: AsyncResult<'a, 'e>,
                asyncResult2: AsyncResult<'b, 'e>
            ) : AsyncResult<'a * 'b, 'e> =
            AsyncResult.zipParallel asyncResult1 asyncResult2

    let reader<'r, 'e> = ReaderBuilder<'r, 'e>()
