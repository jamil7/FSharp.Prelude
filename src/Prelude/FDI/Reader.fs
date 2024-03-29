namespace Prelude.Operators.Reader

[<AutoOpen>]
module ReaderOperators =

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (reader: 'r -> 'a) : 'r -> 'b = fun r -> f (reader r)

    /// Infix apply operator.
    let inline (<*>) (f: 'r -> 'a -> 'b) (reader: 'r -> 'a) : 'r -> 'b = fun r -> f r (reader r)

    /// Infix bind operator.
    let inline (>>=) (reader: 'r -> 'a) (f: 'a -> 'r -> 'b) : 'r -> 'b = fun r -> f (reader r) r


namespace Prelude

open Prelude.Operators.Reader

type Reader<'r, 'a> = 'r -> 'a

[<RequireQualifiedAccess>]
module Reader =

    let singleton (value: 'a) : Reader<'r, 'a> = fun _ -> value

    let map (f: 'a -> 'b) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = f <!> reader

    let apply (f: Reader<'r, 'a -> 'b>) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = f <*> reader

    let bind (f: 'a -> Reader<'r, 'b>) (reader: Reader<'r, 'a>) : Reader<'r, 'b> = reader >>= f

    let run (environment: 'r) (reader: Reader<'r, 'a>) : 'a = reader environment

    let ask: Reader<'e, 'e> = id

    let asks (f: 'r -> 'a) : Reader<'r, 'a> = f <!> ask

    let withReader (f: 'r1 -> 'r2) (reader: Reader<'r2, 'a>) : Reader<'r1, 'a> = f >> reader

[<AutoOpen>]
module ReaderCE =

    type ReaderBuilder() =
        member _.Return(a: 'a) : Reader<'r, 'a> = Reader.singleton a

        member _.ReturnFrom(a: Reader<'r, 'a>) : Reader<'r, 'a> = a

        member _.Zero() : Reader<'r, unit> = Reader.singleton ()

        member _.Bind(reader: Reader<'r, 'a>, f: 'a -> Reader<'r, 'b>) : Reader<'r, 'b> = Reader.bind f reader

        member _.Delay(f: unit -> Reader<'r, 'a>) : Reader<'r, 'a> = Reader.bind f (Reader.singleton ())

        member _.Combine(unitReader: Reader<'r, unit>, reader: Reader<'r, 'a>) : Reader<'r, 'a> =
            Reader.bind (fun () -> reader) unitReader

        member _.TryWith(reader: Reader<'r, 'a>, f: exn -> Reader<'r, 'a>) : Reader<'r, 'a> =
            fun r ->
                try
                    reader r
                with
                | e -> (f e) r

        member _.TryFinally(reader: Reader<'r, 'a>, f: unit -> unit) : Reader<'r, 'a> =
            fun r ->
                try
                    reader r
                finally
                    f ()

        member this.Using(disposable: 'a :> #System.IDisposable, f: 'a -> Reader<'r, 'b>) : Reader<'r, 'b> =
            this.TryFinally(
                f disposable,
                fun () ->
                    if not (obj.ReferenceEquals(disposable, null)) then
                        disposable.Dispose()
            )

        member _.BindReturn(reader: Reader<'r, 'a>, f: 'a -> 'b) : Reader<'r, 'b> = Reader.map f reader

        member this.While(f: unit -> bool, reader: Reader<'r, unit>) : Reader<'r, unit> =
            if not (f ()) then
                Reader.singleton ()
            else
                reader
                |> Reader.bind (fun () -> this.While(f, reader))

        member this.For(sequence: #seq<'a>, f: 'a -> Reader<'r, unit>) =
            this.Using(
                sequence.GetEnumerator(),
                fun enumerator -> this.While(enumerator.MoveNext, this.Delay(fun () -> f enumerator.Current))
            )

    let reader = ReaderBuilder()
