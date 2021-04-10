module FSharp.Prelude.Operators.Async

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
let inline (>>=) (f: 'a -> Async<'b>) (asyncOp: Async<'a>) : Async<'b> = async.Bind(asyncOp, f)
