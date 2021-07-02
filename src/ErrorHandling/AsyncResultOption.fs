namespace Prelude.Operators.AsyncResultOption

open Prelude.ErrorHandling
open Prelude.Extensions

[<AutoOpen>]
module AsyncResultOptionOperators =
    
    let (<!>) (f: 'a -> 'b) (asyncResultOption: AsyncResult<'a option, 'e>) : AsyncResult<'b option, 'e> =
        (Option.map >> AsyncResult.map) f asyncResultOption

    let (<*>)
        (f: AsyncResult<('a -> 'b) option, 'e>)
        (asyncResultOption: AsyncResult<'a option, 'e>)
        : AsyncResult<'b option, 'e> =
        asyncResult {
            let! f' = f
            let! asyncResultOption' = asyncResultOption
            return Option.apply f' asyncResultOption'
        }

    let (<&>)
        (f: AsyncResult<('a -> 'b) option, 'e>)
        (asyncResultOption: AsyncResult<'a option, 'e>)
        : AsyncResult<'b option, 'e> =
        asyncResult {
            let! f' = f
            and! asyncResultOption' = asyncResultOption
            return Option.apply f' asyncResultOption'
        }

    let (>>=) (asyncResultOption: AsyncResult<'a option, 'e>) (f: 'a -> AsyncResult<'b option, 'e>) =
        AsyncResult.bind
            (function
            | Some x -> f x
            | None -> AsyncResult.singleton None)
            asyncResultOption

    let inline (<|>)
        (asyncOption1: AsyncResult<'a option, 'e>)
        (asyncOption2: AsyncResult<'a option, 'e>)
        : AsyncResult<'a option, 'e> =
        AsyncResult.map2 Option.alternative asyncOption1 asyncOption2


namespace Prelude.ErrorHandling

open Prelude.Extensions
open Prelude.Operators.AsyncResultOption
open System.Threading.Tasks

type AsyncResultOption<'a, 'e> = AsyncResult<'a option, 'e>

[<RequireQualifiedAccess>]
module AsyncResultOption =
    
    let singleton (value: 'a) : AsyncResultOption<'a, 'e> =
        (Option.singleton >> AsyncResult.singleton) value

    let map (f: 'a -> 'b) (asyncResultOption: AsyncResultOption<'a, 'e>) : AsyncResultOption<'b, 'e> =
        f <!> asyncResultOption

    let apply
        (f: AsyncResultOption<'a -> 'b, 'e>)
        (asyncResultOption: AsyncResultOption<'a, 'e>)
        : AsyncResultOption<'b, 'e> =
        f <*> asyncResultOption

    let applyParallel
        (f: AsyncResultOption<'a -> 'b, 'e>)
        (asyncResultOption: AsyncResultOption<'a, 'e>)
        : AsyncResultOption<'b, 'e> =
        f <&> asyncResultOption

    let bind
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOption: AsyncResultOption<'a, 'e>)
        : AsyncResultOption<'b, 'e> =
        asyncResultOption >>= f

    let mapError (f: 'e1 -> 'e2) (asyncResultOption: AsyncResultOption<'a, 'e1>) : AsyncResultOption<'a, 'e2> =
        AsyncResult.mapError f asyncResultOption

    let bindError
        (f: 'e1 -> AsyncResultOption<'a, 'e2>)
        (asyncResultOption: AsyncResultOption<'a, 'e1>)
        : AsyncResultOption<'a, 'e2> =
        AsyncResult.bindError f asyncResultOption

    let map2
        (f: 'a -> 'b -> 'c)
        (asyncResultOption1: AsyncResultOption<'a, 'e>)
        (asyncResultOption2: AsyncResultOption<'b, 'e>)
        : AsyncResultOption<'c, 'e> =
        f <!> asyncResultOption1 <*> asyncResultOption2

    let andMap
        (asyncResultOption: AsyncResultOption<'a, 'e>)
        (f: AsyncResultOption<'a -> 'b, 'e>)
        : AsyncResultOption<'b, 'e> =
        map2 (|>) asyncResultOption f

    let bimap
        (f: 'a -> 'b)
        (g: 'e1 -> 'e2)
        (asyncResultOption: AsyncResultOption<'a, 'e1>)
        : AsyncResultOption<'b, 'e2> =
        (map f >> mapError g) asyncResultOption

    let rec private traverser (f: 'a -> AsyncResultOption<'b, 'e>) folder state xs =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            async {
                match! folder head state with
                | Ok _ as this -> return! traverser f folder (Async.singleton this) tail
                | Error _ as this -> return this
            }

    let mapM (f: 'a -> AsyncResultOption<'b, 'e>) (asyncResultOptions: 'a list) : AsyncResultOption<'b list, 'e> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) asyncResultOptions

    let traverse (f: 'a -> AsyncResultOption<'b, 'e>) (asyncResultOptions: 'a list) : AsyncResultOption<'b list, 'e> =
        traverser f (fun head tail -> cons <!> f head <*> tail) (singleton []) asyncResultOptions

    let traverseParallel
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        traverser f (fun head tail -> cons <!> f head <&> tail) (singleton []) asyncResultOptions

    let sequence (asyncResultOptions: AsyncResultOption<'a, 'e> list) : AsyncResultOption<'a list, 'e> =
        mapM id asyncResultOptions

    let sequenceA (asyncResultOptions: AsyncResultOption<'a, 'e> list) : AsyncResultOption<'a list, 'e> =
        traverse id asyncResultOptions

    let sequenceAParallel (asyncResultOptions: AsyncResultOption<'a, 'e> list) : AsyncResultOption<'a list, 'e> =
        traverseParallel id asyncResultOptions

    let zip
        (asyncResultOption1: AsyncResultOption<'a, 'e>)
        (asyncResultOption2: AsyncResultOption<'b, 'e>)
        : AsyncResultOption<'a * 'b, 'e> =
        (fun a b -> a, b) <!> asyncResultOption1
        <*> asyncResultOption2

    let zipParallel
        (asyncResultOption1: AsyncResultOption<'a, 'e>)
        (asyncResultOption2: AsyncResultOption<'b, 'e>)
        : AsyncResultOption<'a * 'b, 'e> =
        (fun a b -> a, b) <!> asyncResultOption1
        <&> asyncResultOption2

    let ofAsyncResult (asyncRes: AsyncResult<'a, 'b>) : AsyncResultOption<'a, 'b> =
        asyncResult {
            let! asyncRes' = asyncRes
            return Some asyncRes'
        }

    let ofAsync (asyncOp: Async<'a>) : AsyncResultOption<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice
        |> ofAsyncResult

    let ofOption (option: 'a option) : AsyncResultOption<'a, 'e> = AsyncResult.singleton option

    let ofResult (result: Result<'a, 'e>) : AsyncResultOption<'a, 'e> = Async.singleton result |> ofAsyncResult

    let ofTask (lazyTask: unit -> Task<'a>) : AsyncResultOption<'a, exn> =
        AsyncResult.ofTask lazyTask |> ofAsyncResult

    let ofUnitTask (lazyUnitTask: unit -> Task) : AsyncResultOption<unit, exn> =
        AsyncResult.ofUnitTask lazyUnitTask
        |> ofAsyncResult


[<AutoOpen>]
module AsyncResultOptionCE =

    type AsyncResultOptionBuilder() =
        member this.Return(value: 'a) : AsyncResultOption<'a, 'e> = AsyncResultOption.singleton value

        member this.ReturnFrom(asyncResultOption: AsyncResultOption<'a, 'e>) : AsyncResultOption<'a, 'e> =
            asyncResultOption

        member this.Zero() : AsyncResultOption<unit, 'e> = AsyncResultOption.singleton ()

        member this.Bind
            (
                asyncResultOption: AsyncResultOption<'a, 'e>,
                f: 'a -> AsyncResultOption<'b, 'e>
            ) : AsyncResultOption<'b, 'e> =
            AsyncResultOption.bind f asyncResultOption

        member this.Delay(f: unit -> AsyncResultOption<'a, 'e>) : AsyncResultOption<'a, 'e> = async.Delay f

        member this.Combine
            (
                unitAsyncResultOption: AsyncResultOption<unit, 'e>,
                asyncResultOption: AsyncResultOption<'a, 'e>
            ) : AsyncResultOption<'a, 'e> =
            AsyncResultOption.bind (fun () -> asyncResultOption) unitAsyncResultOption

        member this.TryWith
            (
                asyncResultOption: AsyncResultOption<'a, 'e>,
                f: exn -> AsyncResultOption<'a, 'e>
            ) : AsyncResultOption<'a, 'e> =
            async.TryWith(asyncResultOption, f)

        member this.TryFinally
            (
                asyncResultOption: AsyncResultOption<'a, 'e>,
                f: unit -> unit
            ) : AsyncResultOption<'a, 'e> =
            async.TryFinally(asyncResultOption, f)

        member this.Using
            (
                disposable: 'a :> System.IDisposable,
                f: 'a -> AsyncResultOption<'b, 'e>
            ) : AsyncResultOption<'b, 'e> =
            async.Using(disposable, f)

        member this.BindReturn(asyncResultOption: AsyncResultOption<'a, 'e>, f: 'a -> 'b) : AsyncResultOption<'b, 'e> =
            AsyncResultOption.map f asyncResultOption

        member this.While
            (
                f: unit -> bool,
                asyncResultOption: AsyncResultOption<unit, 'e>
            ) : AsyncResultOption<unit, 'e> =
            if not (f ()) then
                this.Zero()
            else
                asyncResultOption
                |> AsyncResultOption.bind (fun () -> this.While(f, asyncResultOption))

        member this.MergeSources
            (
                asyncResultOption1: AsyncResultOption<'a, 'e>,
                asyncResultOption2: AsyncResultOption<'b, 'e>
            ) : AsyncResultOption<'a * 'b, 'e> =
            AsyncResultOption.zipParallel asyncResultOption1 asyncResultOption2

        member inline this.Source(asyncResultOption: AsyncResultOption<'a, 'e>) : AsyncResultOption<'a, 'e> =
            asyncResultOption

    let asyncResultOption = AsyncResultOptionBuilder()

[<AutoOpen>]
module AsyncResultOptionCEExtensions =
    type AsyncResultOptionBuilder with
        member inline this.Source(asyncOp: Async<'a>) : AsyncResultOption<'a, exn> = AsyncResultOption.ofAsync asyncOp

        member inline this.Source(result: Result<'a, 'e>) : AsyncResultOption<'a, 'e> =
            AsyncResultOption.ofResult result

        member inline this.Source(task: Task<'a>) : AsyncResultOption<'a, exn> =
            AsyncResultOption.ofTask (fun () -> task)

        member inline this.Source(unitTask: Task) : AsyncResultOption<unit, exn> =
            AsyncResultOption.ofUnitTask (fun () -> unitTask)
