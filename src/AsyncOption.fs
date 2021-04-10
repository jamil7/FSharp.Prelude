namespace FSharp.Prelude.Operators.AsyncOption

open FSharp.Prelude

[<AutoOpen>]
module AsyncOptionOperators =
    let (!>) (value: 'a) : Async<'a option> =
        (Option.singleton >> Async.singleton) value

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

    let inline (>=>) (f: 'a -> Async<'b option>) (g: 'b -> Async<'c option>) : 'a -> Async<'c option> =
        fun x ->
            async {
                let! f' = f x

                let! g' =
                    match f' with
                    | Some thing -> g thing
                    | None -> Async.singleton None

                return g'
            }

    let inline (<|>) (asyncOption1: Async<'a option>) (asyncOption2: Async<'a option>) : Async<'a Option> =
        Async.map2 Option.alternative asyncOption1 asyncOption2


namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncOption
open System.Threading.Tasks

type AsyncOption<'a> = Async<'a option>

[<RequireQualifiedAccess>]
module List =
    let traverseAsyncOptionM (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun head' -> tail >>= (fun tail' -> !>(List.cons head' tail'))))
            asyncOptions
            (!> [])

    let traverseAsyncOptionA (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        List.foldBack (fun head tail -> List.cons <!> f head <*> tail) asyncOptions (!> [])

    let traverseAsyncOptionAParallel (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        List.foldBack (fun head tail -> List.cons <!> f head <&> tail) asyncOptions (!> [])

    let sequenceAsyncOptionM (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        traverseAsyncOptionM id asyncOptions

    let sequenceAsyncOptionA (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        traverseAsyncOptionA id asyncOptions

    let sequenceAsyncOptionAParallel (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        traverseAsyncOptionAParallel id asyncOptions


[<RequireQualifiedAccess>]
module AsyncOption =
    let singleton (value: 'a) : AsyncOption<'a> = !>value

    let map (f: 'a -> 'b) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <!> asyncOption

    let apply (f: AsyncOption<'a -> 'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <*> asyncOption

    let applyParallel (f: AsyncOption<'a -> 'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = f <&> asyncOption

    let bind (f: 'a -> AsyncOption<'b>) (asyncOption: AsyncOption<'a>) : AsyncOption<'b> = asyncOption >>= f

    let alternative (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'a>) : AsyncOption<'a> =
        asyncOption1 <|> asyncOption2

    let map2 (f: 'a -> 'b -> 'c) (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>) : AsyncOption<'c> =
        f <!> asyncOption1 <*> asyncOption2

    let andMap (asyncOption: AsyncOption<'a>) (f: AsyncOption<'a -> 'b>) : AsyncOption<'b> = map2 (|>) asyncOption f

    let compose (f: 'a -> Async<'b option>) (g: 'b -> Async<'c option>) : 'a -> Async<'c option> = f >=> g

    let sequence (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> = List.sequenceAsyncOptionM asyncOptions

    let parallel' (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        async {
            let! array = Async.Parallel asyncOptions
            return Option.sequence (List.ofArray array)
        }

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
        member _.Return(value: 'a) : AsyncOption<'a> = AsyncOption.singleton value

        member _.ReturnFrom(asyncOption: AsyncOption<'a>) : AsyncOption<'a> = asyncOption

        member _.Zero() : AsyncOption<unit> = AsyncOption.singleton ()

        member _.Bind(asyncOption: AsyncOption<'a>, f: 'a -> AsyncOption<'b>) : AsyncOption<'b> =
            AsyncOption.bind f asyncOption

        member _.Delay(f: unit -> AsyncOption<'a>) : AsyncOption<'a> = async.Delay f

        member _.Combine(unitAsyncOption: AsyncOption<unit>, asyncOption: AsyncOption<'a>) : AsyncOption<'a> =
            AsyncOption.bind (fun () -> asyncOption) unitAsyncOption

        member _.TryWith(asyncOption: AsyncOption<'a>, f: exn -> AsyncOption<'a>) : AsyncOption<'a> =
            async.TryWith(asyncOption, f)

        member _.TryFinally(asyncOption: AsyncOption<'a>, f: unit -> unit) : AsyncOption<'a> =
            async.TryFinally(asyncOption, f)

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> AsyncOption<'a>) : AsyncOption<'a> =
            async.Using(disposable, f)

        member this.While(f: unit -> bool, asyncOption: AsyncOption<unit>) : AsyncOption<unit> =
            if not (f ()) then
                this.Zero()
            else
                asyncOption
                |> AsyncOption.bind (fun () -> this.While(f, asyncOption))

        member _.BindReturn(asyncOption: AsyncOption<'a>, f: 'a -> 'b) : AsyncOption<'b> = AsyncOption.map f asyncOption

        member _.MergeSources(asyncOption1: AsyncOption<'a>, asyncOption2: AsyncOption<'b>) : AsyncOption<'a * 'b> =
            AsyncOption.zipParallel asyncOption1 asyncOption2

        member inline _.Source(asyncOption: AsyncOption<'a>) : AsyncOption<'a> = asyncOption

    let asyncOption = AsyncOptionBuilder()

[<AutoOpen>]
module AsyncOptionCEExtensions =
    type AsyncOptionBuilder with
        member inline _.Source(asyncOp: Async<'a>) : AsyncOption<'a> = AsyncOption.ofAsync asyncOp

        member inline _.Source(option: 'a option) : AsyncOption<'a> = AsyncOption.ofOption option

        member inline _.Source(task: Task<'a>) : AsyncOption<'a> = AsyncOption.ofTask (fun () -> task)

        member inline _.Source(unitTask: Task) : AsyncOption<unit> =
            AsyncOption.ofUnitTask (fun () -> unitTask)
