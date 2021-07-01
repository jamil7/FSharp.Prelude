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

type RR<'r, 'a, 'e> = ReaderResult<'r, 'a, 'e>

[<RequireQualifiedAccess>]
module ReaderResult =

    open FSharp.Prelude.Operators.ReaderResult

    let singleton (x: 'a) : RR<'r, 'a, 'e> = fun _ -> Result.singleton x

    let map (f: 'a -> 'b) (rr: RR<'r, 'a, 'e>) : RR<'r, 'b, 'e> = f <!> rr

    let apply (f: RR<'r, 'a -> 'b, 'e>) (rr: RR<'r, 'a, 'e>) : RR<'r, 'b, 'e> = f <*> rr

    let bind (f: 'a -> RR<'r, 'b, 'e>) (rr: RR<'r, 'a, 'e>) : RR<'r, 'b, 'e> = rr >>= f

    let run (environment: 'r) (rr: RR<'r, 'a, 'e>) : Result<'a, 'e> = rr environment

    let ask<'r, 'a, 'e> : RR<'r, 'r, 'e> = Result.singleton >> id

    let asks (f: 'r -> 'a) : RR<'r, 'a, 'e> = f <!> ask

    let withReader (f: 'r1 -> 'r2) (reader: RR<'r2, 'a, 'e>) : RR<'r1, 'a, 'e> = f >> reader
