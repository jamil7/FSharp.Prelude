namespace Prelude.Operators.TaskResult

open System.Threading.Tasks
open Prelude.Extensions

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

namespace Prelude.ErrorHandling

open Ply
open Prelude.Extensions
open Prelude.Operators.TaskResult
open System.Threading.Tasks

type TaskResult<'a, 'e> = Task<Result<'a, 'e>>

[<RequireQualifiedAccess>]
module TaskResult =

    /// Wraps a value in an TaskResult.
    let singleton (value: 'a) : TaskResult<'a, 'e> =
        (Result.singleton >> Task.singleton) value

    let map (f: 'a -> 'b) (asyncResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = f <!> asyncResult

    let apply (f: TaskResult<'a -> 'b, 'e>) (asyncResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = f <*> asyncResult

    let bind (f: 'a -> TaskResult<'b, 'e>) (asyncResult: TaskResult<'a, 'e>) : TaskResult<'b, 'e> = asyncResult >>= f

    let mapError (f: 'e1 -> 'e2) (asyncResult: TaskResult<'a, 'e1>) : TaskResult<'a, 'e2> =
        Task.map (Result.mapError f) asyncResult

    let bindError (f: 'e1 -> TaskResult<'a, 'e2>) (asyncResult: TaskResult<'a, 'e1>) : TaskResult<'a, 'e2> =
        Task.bind
            (function
            | Ok ok -> singleton ok
            | Error error -> f error)
            asyncResult

    let map2
        (f: 'a -> 'b -> 'c)
        (asyncResult1: TaskResult<'a, 'e>)
        (asyncResult2: TaskResult<'b, 'e>)
        : TaskResult<'c, 'e> =
        f <!> asyncResult1 <*> asyncResult2

    let andMap (asyncResult: TaskResult<'a, 'e>) (f: TaskResult<'a -> 'b, 'e>) : TaskResult<'b, 'e> =
        map2 (|>) asyncResult f

    let bimap (f: 'a -> 'b) (g: 'e1 -> 'e2) (asyncResult: TaskResult<'a, 'e1>) : TaskResult<'b, 'e2> =
        (map f >> mapError g) asyncResult

    let rec private traverser
        (f: 'a -> TaskResult<'b, 'e>)
        (folder: 'a -> TaskResult<'b list, 'c> -> TaskResult<'b list, 'c>)
        state
        xs
        : Task<Result<'b list, 'c>> =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            task {
                match! folder head state with
                | Ok _ as this -> return! traverser f folder (Task.singleton this) tail
                | Error _ as this -> return this
            }

    let mapM (f: 'a -> TaskResult<'b, 'e>) (asyncResults: 'a list) : TaskResult<'b list, 'e> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) asyncResults

    let traverse (f: 'a -> TaskResult<'b, 'e>) (asyncResults: 'a list) : TaskResult<'b list, 'e> =
        traverser f (fun head tail -> cons <!> f head <*> tail) (singleton []) asyncResults

    let sequence (asyncResults: TaskResult<'a, 'e> list) : TaskResult<'a list, 'e> = mapM id asyncResults

    let sequenceA (asyncResults: TaskResult<'a, 'e> list) : TaskResult<'a list, 'e> = traverse id asyncResults

    let zip (asyncResult1: TaskResult<'a, 'e>) (asyncResult2: TaskResult<'b, 'e>) : TaskResult<'a * 'b, 'e> =
        (fun a b -> a, b) <!> asyncResult1
        <*> asyncResult2

    let ofOption (error: 'e) (option: 'a option) : TaskResult<'a, 'e> =
        Task.singleton (Result.ofOption error option)

    let ofResult (result: Result<'a, 'e>) : TaskResult<'a, 'e> = Task.singleton result

    let ofChoice (choice: Choice<'a, 'e>) : TaskResult<'a, 'e> = Task.singleton (Result.ofChoice choice)

    let ofTask (taskOp: Task<'a>) : TaskResult<'a, exn> =
        taskOp |> Task.Catch |> Task.map Result.ofChoice

    let ofUnitTask (unitTask: Task) : TaskResult<unit, exn> = unitTask |> Task.ofUnitTask |> ofTask

    let ofAsync (asyncOp: Async<'a>) : TaskResult<'a, exn> = asyncOp |> Async.StartAsTask |> ofTask

[<AutoOpen>]
module AsyncResultCE =
    type AsyncResultBuilder() =
        member _.Return(value: 'a) : TaskResult<'a, 'e> = TaskResult.singleton value

        member _.ReturnFrom(taskResult: TaskResult<'a, 'e>) : TaskResult<'a, 'e> = taskResult

        member _.Zero() : TaskResult<unit, 'e> = TaskResult.singleton ()

        member _.Bind(taskResult: TaskResult<'a, 'e>, f: 'a -> TaskResult<'b, 'e>) : TaskResult<'b, 'e> =
            TaskResult.bind f taskResult

        member _.Delay(f: unit -> Ply<Result<'a, 'e>>) : unit -> Ply<Result<'a, 'e>> = task.Delay f

        member _.Run(f: unit -> Ply<'a>) : Task<'a> = task.Run f

        member _.Combine(unitAsyncResult: TaskResult<unit, 'e>, taskResult: TaskResult<'a, 'e>) : TaskResult<'a, 'e> =
            TaskResult.bind (fun () -> taskResult) unitAsyncResult

        member _.TryWith(taskResult: unit -> Ply<Result<'a, 'e>>, f: exn -> Ply<Result<'a, 'e>>) : Ply<Result<'a, 'e>> =
            task.TryWith(taskResult, f)

        member _.TryFinally(taskResult: unit -> Ply<Result<'a, 'e>>, f: unit -> unit) : Ply<Result<'a, 'e>> =
            task.TryFinally(taskResult, f)

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> Ply<Result<'b, 'e>>) : Ply<Result<'b, 'e>> =
            task.Using(disposable, f)

        member _.BindReturn(taskResult: TaskResult<'a, 'e>, f: 'a -> 'b) : TaskResult<'b, 'e> =
            TaskResult.map f taskResult

        member this.While(f: unit -> bool, taskResult: TaskResult<unit, 'e>) : TaskResult<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                taskResult
                |> TaskResult.bind (fun () -> this.While(f, taskResult))

        member _.MergeSources
            (
                asyncResult1: TaskResult<'a, 'e>,
                asyncResult2: TaskResult<'b, 'e>
            ) : TaskResult<'a * 'b, 'e> =
            TaskResult.zip asyncResult1 asyncResult2

        member inline _.Source(taskResult: TaskResult<'a, 'e>) : TaskResult<'a, 'e> = taskResult
        
        member inline _.Source(taskResult: Ply<Result<'a, 'e>>) : TaskResult<'a, 'e> = task { return! taskResult }

        member inline _.Source(taskResult: ValueTask<Result<'a, 'e>>) : TaskResult<'a, 'e> = task { return! taskResult }

    let taskResult = AsyncResultBuilder()

[<AutoOpen>]
module AsyncResultCEExtensions =
    type AsyncResultBuilder with
        member inline _.Source(asyncOp: Async<'a>) : TaskResult<'a, exn> = TaskResult.ofAsync asyncOp

        member inline _.Source(asyncResult: Async<Result<'a, 'e>>) : TaskResult<'a, 'e> = Async.StartAsTask asyncResult

        member inline _.Source(result: Result<'a, 'e>) : TaskResult<'a, 'e> = TaskResult.ofResult result
