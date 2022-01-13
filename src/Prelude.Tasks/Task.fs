namespace Prelude.Operators.Task

open System.Threading.Tasks

[<AutoOpen>]
module TaskOperators =

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (taskOp: Task<'a>) : Task<'b> =
        task {
            let! taskOp' = taskOp
            return f taskOp'
        }

    /// Infix apply operator.
    let inline (<*>) (f: Task<'a -> 'b>) (taskOp: Task<'a>) : Task<'b> =
        task {
            let! f' = f
            let! taskOp' = taskOp
            return f' taskOp'
        }

    /// Infix bind operator.
    let inline (>>=) (taskOp: Task<'a>) (f: 'a -> Task<'b>) : Task<'b> =
        task {
            let! taskOp' = taskOp
            return! f taskOp'
        }

namespace Prelude

open Prelude.Operators.Task
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Task =

    /// Wraps a value in an Task.
    let singleton (value: 'a) : Task<'a> = task { return value }

    let map (f: 'a -> 'b) (taskOp: Task<'a>) : Task<'b> = f <!> taskOp

    let apply (f: Task<'a -> 'b>) (taskOp: Task<'a>) : Task<'b> = f <*> taskOp

    let bind (f: 'a -> Task<'b>) (taskOp: Task<'a>) : Task<'b> = taskOp >>= f

    let map2 (f: 'a -> 'b -> 'c) (taskOp1: Task<'a>) (taskOp2: Task<'b>) : Task<'c> = f <!> taskOp1 <*> taskOp2

    let andMap (taskOp: Task<'a>) (f: Task<'a -> 'b>) : Task<'b> = map2 (|>) taskOp f

    let zip (taskOp1: Task<'a>) (taskOp2: Task<'b>) : Task<'a * 'b> =
        (fun a b -> a, b) <!> taskOp1 <*> taskOp2
