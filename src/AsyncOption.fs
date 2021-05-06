namespace FSharp.Prelude.Operators.AsyncOption

open FSharp.Prelude

[<AutoOpen>]
module AsyncOptionOperators =

    let inline (<!>) (f: 'a -> 'b) (asyncOption: Async<'a option>) : Async<'b option> =
        (Option.map >> Async.map) f asyncOption

    let inline (<*>) (f: Async<('a -> 'b) option>) (asyncOption: Async<'a option>) : Async<'b option> =
        async {
            let! f' = f
            let! asyncOption' = asyncOption
            return Option.apply f' asyncOption'
        }

    let inline (<&>) (f: Async<('a -> 'b) option>) (asyncOption: Async<'a option>) : Async<'b option> =
        async {
            let! f' = f
            and! asyncOption' = asyncOption
            return Option.apply f' asyncOption'
        }

    let inline (>>=) (asyncOption: Async<'a option>) (f: 'a -> Async<'b option>) : Async<'b option> =
        Async.bind
            (function
            | Some something -> f something
            | None -> Async.singleton None)
            asyncOption

    let inline (<|>) (asyncOption1: Async<'a option>) (asyncOption2: Async<'a option>) : Async<'a Option> =
        Async.map2 Option.alternative asyncOption1 asyncOption2


namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncOption
open System.Threading.Tasks

type AsyncOption<'a> = Async<'a option>


[<RequireQualifiedAccess>]
module AsyncOption =
    let singleton (value: 'a) : AsyncOption<'a> =
        (Option.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <!> asyncOption

    let apply (f: AsyncOption<'a -> 'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <*> asyncOption

    let applyParallel (f: AsyncOption<'a -> 'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <&> asyncOption

    let bind (f: 'a -> AsyncOption<'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = asyncOption >>= f

    let alternative (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'a>) : AsyncOption<'a> =
        asyncOption1 <|> asyncOption2

    let map2 (f: 'a -> 'b -> 'c) (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>) : AsyncOption<'c> =
        f <!> asyncOption1 <*> asyncOption2

    let andMap (asyncOption: AsyncOption<'a>) (f: AsyncOption<'a -> 'b>) : AsyncOption<'b> = map2 (|>) asyncOption f

    let rec private traverser f folder state xs =
        match xs with
        | [] -> List.rev <!> state
        | head :: tail ->
            async {
                match! folder head state with
                | Some _ as this -> return! traverser f folder (Async.singleton this) tail
                | None as this -> return this
            }

    let mapM (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        let folder head tail =
            f head
            >>= fun head' ->
                    tail
                    >>= fun tail' -> singleton <| cons head' tail'

        traverser f folder (singleton []) asyncOptions

    let traverse (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        traverser f (fun head tail -> cons <!> f head <*> tail) (singleton []) asyncOptions

    let traverseParallel (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        traverser f (fun head tail -> cons <!> f head <&> tail) (singleton []) asyncOptions

    let sequence (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> = mapM id asyncOptions

    let sequenceA (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> = traverse id asyncOptions

    let sequenceAParallel (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> = traverseParallel id asyncOptions

    let zip (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>) : Async<('a * 'b) option> =
        (fun a b -> a, b) <!> asyncOption1
        <*> asyncOption2

    let zipParallel (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>) : Async<('a * 'b) option> =
        (fun a b -> a, b) <!> asyncOption1
        <&> asyncOption2

    let ofAsync (asyncOp: Async<'a>) : AsyncOption<'a> =
        asyncOp
        |> Async.Catch
        |> Async.map Option.ofChoice

    let ofResult (result: Result<'a, 'b>) : AsyncOption<'a> =
        Async.singleton (Option.ofResult result)

    let ofOption (option: 'a option) : AsyncOption<'a> = Async.singleton option

    let ofTask (lazyTask: unit -> Task<'a>) : AsyncOption<'a> =
        async.Delay(lazyTask >> Async.AwaitTaskWithInnerException)
        |> ofAsync

    let ofUnitTask (lazyTask: unit -> Task) : AsyncOption<unit> =
        async.Delay(lazyTask >> Async.AwaitTaskWithInnerException)
        |> ofAsync

    let ofAsyncResult (asyncResult: AsyncResult<'a, 'b>) : AsyncOption<'a> = Async.bind ofResult asyncResult

[<AutoOpen>]
module AsyncOptionCE =
    type AsyncOptionBuilder() =
        member this.Return(value: 'a) : AsyncOption<'a> = AsyncOption.singleton value

        member this.ReturnFrom(asyncOption: AsyncOption<'a>) : AsyncOption<'a> = asyncOption

        member this.Zero() : AsyncOption<unit> = AsyncOption.singleton ()

        member this.Bind(asyncOption: AsyncOption<'a>, f: 'a -> AsyncOption<'b>) : AsyncOption<'b> =
            AsyncOption.bind f asyncOption

        member this.Delay(f: unit -> AsyncOption<'a>) : AsyncOption<'a> = async.Delay f

        member this.Combine(unitAsyncOption: AsyncOption<unit>, asyncOption: AsyncOption<'a>) : AsyncOption<'a> =
            AsyncOption.bind (fun () -> asyncOption) unitAsyncOption

        member this.TryWith(asyncOption: AsyncOption<'a>, f: exn -> AsyncOption<'a>) : AsyncOption<'a> =
            async.TryWith(asyncOption, f)

        member this.TryFinally(asyncOption: AsyncOption<'a>, f: unit -> unit) : AsyncOption<'a> =
            async.TryFinally(asyncOption, f)

        member this.Using(disposable: 'a :> System.IDisposable, f: 'a -> AsyncOption<'a>) : AsyncOption<'a> =
            async.Using(disposable, f)

        member this.While(f: unit -> bool, asyncOption: AsyncOption<unit>) : AsyncOption<unit> =
            if not (f ()) then
                this.Zero()
            else
                asyncOption
                |> AsyncOption.bind (fun () -> this.While(f, asyncOption))

        member this.BindReturn(asyncOption: AsyncOption<'a>, f: 'a -> 'b) : AsyncOption<'b> =
            AsyncOption.map f asyncOption

        member this.MergeSources(asyncOption1: AsyncOption<'a>, asyncOption2: AsyncOption<'b>) : AsyncOption<'a * 'b> =
            AsyncOption.zipParallel asyncOption1 asyncOption2

        member inline this.Source(asyncOption: AsyncOption<'a>) : AsyncOption<'a> = asyncOption

    let asyncOption = AsyncOptionBuilder()

[<AutoOpen>]
module AsyncOptionCEExtensions =
    type AsyncOptionBuilder with
        member inline this.Source(asyncOp: Async<'a>) : AsyncOption<'a> = AsyncOption.ofAsync asyncOp

        member inline this.Source(option: 'a option) : AsyncOption<'a> = AsyncOption.ofOption option

        member inline this.Source(task: Task<'a>) : AsyncOption<'a> = AsyncOption.ofTask (fun () -> task)

        member inline this.Source(unitTask: Task) : AsyncOption<unit> =
            AsyncOption.ofUnitTask (fun () -> unitTask)
