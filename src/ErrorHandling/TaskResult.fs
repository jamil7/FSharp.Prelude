namespace Prelude.Operators.TaskResult

open Prelude
open System.Threading.Tasks

[<AutoOpen>]
module TaskResultOperators =

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (taskResult: Task<Result<'a, 'e>>) : Task<Result<'b, 'e>> =
        (Result.map >> Task.map) f taskResult

    /// Infix apply operator.
    let inline (<*>) (f: Task<Result<'a -> 'b, 'e>>) (taskResult: Task<Result<'a, 'e>>) : Task<Result<'b, 'e>> =
        task {
            let! f' = f
            let! taskResult' = taskResult
            return Result.apply f' taskResult'
        }

    /// Infix bind operator.
    let inline (>>=) (taskResult: Task<Result<'a, 'e>>) (f: 'a -> Task<Result<'b, 'e>>) : Task<Result<'b, 'e>> =
        Task.bind
            (function
            | Ok ok -> f ok
            | Error error -> Task.singleton (Error error))
            taskResult

namespace Prelude

open Prelude.Operators.TaskResult
open System.Threading.Tasks

type TaskResult<'a, 'e> = Task<Result<'a, 'e>>

[<RequireQualifiedAccess>]
module TaskResult =

    /// Wraps a value in an TaskResult.
    let singleton (value: 'a) : TaskResult<'a, 'e> =
        (Result.singleton >> Task.singleton) value

    let map (f: 'a -> 'b) (taskResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = f <!> taskResult

    let apply (f: TaskResult<'a -> 'b, 'e>) (taskResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = f <*> taskResult

    let bind (f: 'a -> TaskResult<'b, 'e>) (taskResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = taskResult >>= f

    let mapError (f: 'e1 -> 'e2) (taskResult: TaskResult<'a, 'e1>) : TaskResult<'a, 'e2> =
        Task.map (Result.mapError f) taskResult

    let bindError (f: 'e1 -> TaskResult<'a, 'e2>) (taskResult: TaskResult<'a, 'e1>) : TaskResult<'a, 'e2> =
        Task.bind
            (function
            | Ok ok -> singleton ok
            | Error error -> f error)
            taskResult

    let map2
        (f: 'a -> 'b -> 'c)
        (taskResult1: TaskResult<'a, 'e>)
        (taskResult2: TaskResult<'b, 'e>)
        : TaskResult<'c, 'e> =
        f <!> taskResult1 <*> taskResult2

    let andMap (taskResult: TaskResult<'a, 'e>) (f: TaskResult<'a -> 'b, 'e>) : TaskResult<'b, 'e> =
        map2 (|>) taskResult f

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (taskResult: TaskResult<'a, 'e1>) : TaskResult<'b, 'e2> =
        (map f >> mapError g) taskResult

    let rec private traverser
        (f: 'a -> TaskResult<'b, 'e>)
        (folder: 'a -> TaskResult<'b list, 'e> -> TaskResult<'b list, 'e>)
        state
        xs
        =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            task {
                match! folder head state with
                | Ok _ as this -> return! traverser f folder (Task.singleton this) tail
                | Error _ as this -> return this
            }

    let mapM (f: 'a -> TaskResult<'b, 'e>) (taskResults: 'a list) : TaskResult<'b list, 'e> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) taskResults

    let traverse (f: 'a -> TaskResult<'b, 'e>) (taskResults: 'a list) : TaskResult<'b list, 'e> =
        traverser f (fun head tail -> cons <!> f head <*> tail) (singleton []) taskResults

    let sequence (taskResults: TaskResult<'a, 'e> list) : TaskResult<'a list, 'e> = mapM id taskResults

    let sequenceA (taskResults: TaskResult<'a, 'e> list) : TaskResult<'a list, 'e> = traverse id taskResults

    let zip (taskResult1: TaskResult<'a, 'e>) (taskResult2: TaskResult<'b, 'e>) : TaskResult<'a * 'b, 'e> =
        (fun a b -> a, b) <!> taskResult1 <*> taskResult2

    let ofAsync (asyncOp: Async<'a>) : TaskResult<'a, exn> =
        task {
            let! choice = Async.Catch asyncOp
            return Result.ofChoice choice
        }

    let ofOption (error: 'e) (option: 'a option) : TaskResult<'a, 'e> =
        Task.singleton (Result.ofOption error option)

    let ofResult (result: Result<'a, 'e>) : TaskResult<'a, 'e> = Task.singleton result

    let ofChoice (choice: Choice<'a, 'e>) : TaskResult<'a, 'e> = Task.singleton (Result.ofChoice choice)

    let ofTask (lazyTask: unit -> Task<'a>) : TaskResult<'a, exn> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> ofAsync

    let ofUnitTask (lazyUnitTask: unit -> Task) : TaskResult<unit, exn> =
        async.Delay(lazyUnitTask >> Async.AwaitTask)
        |> ofAsync
