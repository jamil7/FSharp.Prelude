namespace FSharp.Prelude.Operators.AsyncResultOption

open FSharp.Prelude

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
