namespace FSharp.Prelude.Operators.ReaderResult

[<AutoOpen>]
module ResultReader =

    open FSharp.Prelude

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (rr: 'r -> Async<Result<'a, 'err>>) : 'r -> Async<Result<'b, 'err>> =
        fun r -> AsyncResult.map f (rr r)

    /// Infix apply operator.
    let inline (<*>)
        (f: 'r -> Async<Result<'a -> 'b, 'e>>)
        (rr: 'r -> Async<Result<'a, 'e>>)
        : 'r -> Async<Result<'b, 'e>> =
        fun e -> AsyncResult.apply (f e) (rr e)

    /// Infix bind operator.
    let inline (>>=)
        (rr: 'r -> Async<Result<'a, 'e>>)
        (f: 'a -> 'r -> Async<Result<'b, 'e>>)
        : 'r -> Async<Result<'b, 'e>> =
        fun e -> AsyncResult.bind (fun a -> f a e) (rr e)

namespace FSharp.Prelude

type ReaderAsyncResult<'env, 'a, 'err> = 'env -> AsyncResult<'a, 'err>

type RAR<'env, 'a, 'err> = ReaderAsyncResult<'env, 'a, 'err>

[<RequireQualifiedAccess>]
module ReaderAsyncResult =

    open FSharp.Prelude.Operators.ReaderResult

    let singleton (x: 'a) : RAR<'r, 'a, 'e> = fun _ -> AsyncResult.singleton x

    let map (f: 'a -> 'b) (rar: RAR<'r, 'a, 'e>) : RAR<'r, 'b, 'e> = f <!> rar

    let apply (f: RAR<'r, 'a -> 'b, 'e>) (rar: RAR<'r, 'a, 'e>) : RAR<'r, 'b, 'e> = f <*> rar

    let bind (f: 'a -> RAR<'r, 'b, 'e>) (rar: RAR<'r, 'a, 'e>) : RAR<'r, 'b, 'e> = rar >>= f

    let run (environment: 'r) (rar: RAR<'r, 'a, 'e>) : AsyncResult<'a, 'e> = rar environment

    let ask<'r, 'a, 'e> : RAR<'r, 'r, 'e> = AsyncResult.singleton >> id

    let asks (f: 'r -> 'a) : RAR<'r, 'a, 'e> = f <!> ask

    let withReader (f: 'r1 -> 'r2) (rar: RAR<'r2, 'a, 'e>) : RAR<'r1, 'a, 'e> = f >> rar
