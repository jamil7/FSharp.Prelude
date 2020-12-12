namespace FSharp.Prelude.Operators.AsyncResultOption

open FSharp.Prelude

[<AutoOpen>]
module AsyncResultOptionOperators =
    let (<!>) (f: 'a -> 'b) (asyncResultOption: AsyncResult<'a option, 'e>): AsyncResult<'b option, 'e> =
        (Option.map >> AsyncResult.map) f asyncResultOption

    let (<*>) (f: AsyncResult<('a -> 'b) option, 'e>)
              (asyncResultOption: AsyncResult<'a option, 'e>)
              : AsyncResult<'b option, 'e> =
        asyncResult {
            let! f' = f
            and! asyncResultOption' = asyncResultOption
            return Option.apply f' asyncResultOption'
        }

    let (>>=) (f: 'a -> AsyncResult<'b option, 'e>) (asyncResultOption: AsyncResult<'a option, 'e>) =
        AsyncResult.bind (function
            | Some x -> f x
            | None -> AsyncResult.singleton None) asyncResultOption

    let inline (>=>) (f: 'a -> AsyncResult<'b option, 'e>)
                     (g: 'b -> AsyncResult<'c option, 'e>)
                     : 'a -> AsyncResult<'c option, 'e> =
        fun x ->
            asyncResult {
                let! f' = f x

                let! g' =
                    match f' with
                    | Some thing -> g thing
                    | None -> AsyncResult.singleton None

                return g'
            }

    let inline (<|>) (asyncOption1: AsyncResult<'a option, 'e>)
                     (asyncOption2: AsyncResult<'a option, 'e>)
                     : AsyncResult<'a option, 'e> =
        AsyncResult.map2 Option.alternative asyncOption1 asyncOption2


namespace FSharp.Prelude

open FSharp.Prelude.Operators.AsyncResultOption
open System.Threading.Tasks

type AsyncResultOption<'a, 'e> = AsyncResult<'a option, 'e>

[<RequireQualifiedAccess>]
module AsyncResultOption =
    let singleton (value: 'a): AsyncResultOption<'a, 'e> =
        (Option.singleton >> AsyncResult.singleton) value

    let map (f: 'a -> 'b) (asyncResultOption: AsyncResultOption<'a, 'e>): AsyncResultOption<'b, 'e> =
        f <!> asyncResultOption

    let apply (f: AsyncResultOption<('a -> 'b), 'e>)
              (asyncResultOption: AsyncResultOption<'a, 'e>)
              : AsyncResultOption<'b, 'e> =
        f <*> asyncResultOption

    let bind (f: 'a -> AsyncResultOption<'b, 'e>)
             (asyncResultOption: AsyncResultOption<'a, 'e>)
             : AsyncResultOption<'b, 'e> =
        f >>= asyncResultOption

    let mapError (f: 'e1 -> 'e2) (asyncResultOption: AsyncResultOption<'a, 'e1>): AsyncResultOption<'a, 'e2> =
        AsyncResult.mapError f asyncResultOption

    let bindError (f: 'e1 -> AsyncResultOption<'a, 'e2>)
                  (asyncResultOption: AsyncResultOption<'a, 'e1>)
                  : AsyncResultOption<'a, 'e2> =
        AsyncResult.bindError f asyncResultOption

    let map2 (f: 'a -> 'b -> 'c)
             (asyncResultOption1: AsyncResultOption<'a, 'e>)
             (asyncResultOption2: AsyncResultOption<'b, 'e>)
             : AsyncResultOption<'c, 'e> =
        f <!> asyncResultOption1 <*> asyncResultOption2

    let andMap (asyncResultOption: AsyncResultOption<'a, 'e>)
               (f: AsyncResultOption<('a -> 'b), 'e>)
               : AsyncResultOption<'b, 'e> =
        map2 (|>) asyncResultOption f

    let compose (f: 'a -> AsyncResultOption<'b, 'e>) (g: 'b -> AsyncResultOption<'c, 'e>): 'a -> AsyncResultOption<'c, 'e> =
        f >=> g

    let sequence (asyncResultOptions: AsyncResultOption<'a, 'e> list): AsyncResultOption<'a list, 'e> =
        List.foldBack (fun asyncResult1 asyncResult2 ->
            (fun head tail -> head :: tail)
            <!> asyncResult1
            <*> asyncResult2) asyncResultOptions (singleton [])

    let zip (asyncResultOption1: AsyncResultOption<'a, 'e>)
            (asyncResultOption2: AsyncResultOption<'b, 'e>)
            : AsyncResultOption<'a * 'b, 'e> =
        (fun a b -> a, b)
        <!> asyncResultOption1
        <*> asyncResultOption2

    let ofAsyncResult (asyncRes: AsyncResult<'a, 'b>): AsyncResultOption<'a, 'b> =
        asyncResult {
            let! asyncRes' = asyncRes
            return Some asyncRes'
        }

    let ofAsync (asyncOp: Async<'a>): AsyncResultOption<'a, exn> =
        asyncOp
        |> Async.Catch
        |> Async.map Result.ofChoice
        |> ofAsyncResult

    let ofOption (error: 'e) (option: 'a option): AsyncResultOption<'a, 'e> = AsyncResult.singleton option

    let ofResult (result: Result<'a, 'e>): AsyncResultOption<'a, 'e> = Async.singleton result |> ofAsyncResult

    let ofTask (lazyTask: unit -> Task<'a>): AsyncResultOption<'a, exn> =
        AsyncResult.ofTask lazyTask |> ofAsyncResult

    let ofUnitTask (lazyUnitTask: unit -> Task): AsyncResultOption<unit, exn> =
        AsyncResult.ofUnitTask lazyUnitTask
        |> ofAsyncResult
