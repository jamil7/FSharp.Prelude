module FSharp.Prelude.Operators.Async

/// Singleton (return) operator.
let (->>) (value: 'a) : Async<'a> = async.Return(value)

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
let inline (>>=) (asyncOp: Async<'a>) (f: 'a -> Async<'b>)  : Async<'b> = async.Bind(asyncOp, f)
