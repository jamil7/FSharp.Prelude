namespace Prelude.Operators.TaskOption

open Prelude
open System.Threading.Tasks

[<AutoOpen>]
module TaskOptionOperators =

    let inline (<!>) (f: 'a -> 'b) (taskOption: Task<'a option>) : Task<'b option> =
        (Option.map >> Task.map) f taskOption

    let inline (<*>) (f: Task<('a -> 'b) option>) (taskOption: Task<'a option>) : Task<'b option> =
        task {
            let! f' = f
            let! taskOption' = taskOption
            return Option.apply f' taskOption'
        }

    let inline (>>=) (taskOption: Task<'a option>) (f: 'a -> Task<'b option>) : Task<'b option> =
        Task.bind
            (function
            | Some something -> f something
            | None -> Task.singleton None)
            taskOption


namespace Prelude

open Prelude.Operators.TaskOption
open System.Threading.Tasks

type TaskOption<'a> = Task<'a option>

[<RequireQualifiedAccess>]
module TaskOption =

    let singleton (value: 'a) : TaskOption<'a> =
        (Option.singleton >> Task.singleton) value

    let map (f: 'a -> 'b) (taskOption: TaskOption<'a>) : TaskOption<'b> = f <!> taskOption

    let apply (f: TaskOption<'a -> 'b>) (taskOption: TaskOption<'a>) : TaskOption<'b> = f <*> taskOption

    let bind (f: 'a -> TaskOption<'b>) (taskOption: TaskOption<'a>) : TaskOption<'b> = taskOption >>= f

    let map2 (f: 'a -> 'b -> 'c) (taskOption1: TaskOption<'a>) (taskOption2: TaskOption<'b>) : TaskOption<'c> =
        f <!> taskOption1 <*> taskOption2

    let andMap (taskOption: TaskOption<'a>) (f: TaskOption<'a -> 'b>) : TaskOption<'b> = map2 (|>) taskOption f

    let rec private traverser
        (f: 'a -> TaskOption<'b>)
        (folder: 'a -> TaskOption<'b list> -> TaskOption<'b list>)
        state
        xs
        =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            task {
                match! folder head state with
                | Some _ as this -> return! traverser f folder (Task.singleton this) tail
                | None as this -> return this
            }

    let traverse (f: 'a -> TaskOption<'b>) (taskOptions: 'a list) : TaskOption<'b list> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) taskOptions

    let sequence (taskOptions: TaskOption<'a> list) : TaskOption<'a list> = traverse id taskOptions

    let zip (taskOption1: TaskOption<'a>) (taskOption2: TaskOption<'b>) : Task<('a * 'b) option> =
        (fun a b -> a, b) <!> taskOption1 <*> taskOption2

    let ofAsync (asyncOp: Async<'a>) : TaskOption<'a> =
        task {
            let! choice = Async.Catch asyncOp
            return Option.ofChoice choice
        }

    let ofResult (result: Result<'a, 'b>) : TaskOption<'a> = Task.singleton (Option.ofResult result)

    let ofOption (option: 'a option) : TaskOption<'a> = Task.singleton option

    let ofTask (lazyTask: unit -> Task<'a>) : TaskOption<'a> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> ofAsync

    let ofUnitTask (lazyTask: unit -> Task) : TaskOption<unit> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> ofAsync

    let ofTaskResult (taskResult: TaskResult<'a, 'b>) : TaskOption<'a> = Task.bind ofResult taskResult
