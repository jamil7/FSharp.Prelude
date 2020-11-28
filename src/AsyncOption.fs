namespace FSharp.Prelude.Operators.AsyncOption

open FSharp.Prelude

[<AutoOpen>]
module AsyncOptionOperators =
    let inline (<!>) (f: 'a -> 'b) (asyncOption: Async<'a option>): Async<'b option> =
        (Option.map >> Async.map) f asyncOption

    let inline (<*>) (f: Async<('a -> 'b) option>) (asyncOption: Async<'a option>): Async<'b option> =
        async {
            let! runF = Async.StartChild f
            let! runAsyncOption = Async.StartChild asyncOption
            let! f' = runF
            let! option = runAsyncOption
            return Option.apply f' option
        }

    let inline (>>=) (f: 'a -> Async<'b option>) (asyncOption: Async<'a option>): Async<'b option> =
        Async.bind (function
            | Some something -> f something
            | None -> Async.singleton None) asyncOption

//    let (<|>) (asyncOption1: Async<Option<'a>>) (asyncOption2: Async<Option<'a>>): Async<Option<'a>> =
//        Option.alternative
//        <!> asyncOption1
//        <*> asyncOption2

namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncOption
open System.Threading.Tasks

type AsyncOption<'a> = Async<'a option>

[<RequireQualifiedAccess>]
module AsyncOption =
    let singleton (value: 'a): AsyncOption<'a> =
        (Option.singleton >> Async.singleton) value

    let map (f: 'a -> 'b) (asyncOption: AsyncOption<'a>): AsyncOption<'b> = f <!> asyncOption

    let apply (f: AsyncOption<'a -> 'b>) (asyncOption: AsyncOption<'a>): AsyncOption<'b> = f <*> asyncOption

    let bind (f: 'a -> AsyncOption<'b>) (asyncOption: AsyncOption<'a>): AsyncOption<'b> = f >>= asyncOption

    //    let alternative (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'a>): AsyncOption<'a> =
//        asyncOption1 <|> asyncOption2

    let map2 (f: 'a -> 'b -> 'c) (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>): AsyncOption<'c> =
        f <!> asyncOption1 <*> asyncOption2

    let andMap (asyncOption: AsyncOption<'a>) (f: AsyncOption<'a -> 'b>): AsyncOption<'b> = map2 (|>) asyncOption f

    let sequence (asyncOptions: AsyncOption<'a> list): AsyncOption<'a list> =
        List.foldr (fun asyncOption1 asyncOption2 -> List.cons <!> asyncOption1 <*> asyncOption2) (singleton [])
            asyncOptions

    let zip (asyncOption1: AsyncOption<'a>) (asyncOption2: AsyncOption<'b>) =
        (fun a b -> a, b)
        <!> asyncOption1
        <*> asyncOption2

    let ofResult (result: Result<'a, 'b>): AsyncOption<'a> = Async.singleton (Option.ofResult result)

    let ofOption (option: 'a option): AsyncOption<'a> = Async.singleton option

    let ofTask (lazyTask: unit -> Task<'a>): AsyncOption<'a> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Option.ofChoice

    let ofUnitTask (lazyTask: unit -> Task): AsyncOption<unit> =
        async.Delay(lazyTask >> Async.AwaitTask)
        |> Async.Catch
        |> Async.map Option.ofChoice

    let ofAsyncResult (asyncResult: AsyncResult<'a, 'b>): AsyncOption<'a> = Async.bind ofResult asyncResult

[<AutoOpen>]
module AsyncOptionCE =
    type AsyncOptionBuilder() =
        member _.Return(value: 'a): AsyncOption<'a> = AsyncOption.singleton value

        member _.ReturnFrom(option: 'a option): AsyncOption<'a> = AsyncOption.ofOption option

        member _.ReturnFrom(asyncOption: AsyncOption<'a>): AsyncOption<'a> = asyncOption

        member _.Zero(): AsyncOption<unit> = AsyncOption.singleton ()

        member _.Bind(option: 'a option, f: 'a -> 'b option): AsyncOption<'b> =
            AsyncOption.ofOption (Option.bind f option)

        member _.Bind(asyncOption: AsyncOption<'a>, f: 'a -> AsyncOption<'b>): AsyncOption<'b> = f >>= asyncOption

        member _.Delay(f: unit -> AsyncOption<'a>): AsyncOption<'a> = async.Delay f

        member _.Combine(unitAsyncOption: AsyncOption<unit>, asyncOption: AsyncOption<'a>): AsyncOption<'a> =
            (fun () -> asyncOption) >>= unitAsyncOption

        member _.TryWith(asyncOption: AsyncOption<'a>, f: exn -> AsyncOption<'a>): AsyncOption<'a> =
            async.TryWith(asyncOption, f)

        member _.TryFinally(asyncOption: AsyncOption<'a>, f: unit -> unit): AsyncOption<'a> =
            async.TryFinally(asyncOption, f)

        member _.Using(disposable: 'a :> System.IDisposable, f: 'a -> AsyncOption<'a>): AsyncOption<'a> =
            async.Using(disposable, f)

        member _.BindReturn(asyncOption: AsyncOption<'a>, f: 'a -> 'b): AsyncOption<'b> = f <!> asyncOption

        member _.MergeSources(asyncOption1: AsyncOption<'a>, asyncOption2: AsyncOption<'b>): AsyncOption<'a * 'b> =
            AsyncOption.zip asyncOption1 asyncOption2

    let asyncOption = AsyncOptionBuilder()
