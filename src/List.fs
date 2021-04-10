namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    let cons head tail = head :: tail

    // AsyncOperations
    
    open FSharp.Prelude.Operators.Async

    let traverseAsyncM (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun h -> tail >>= (fun t -> (->>) (cons h t))))
            asyncOps
            ((->>) [])

    let traverseAsyncA (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack (fun head tail -> cons <!> f head <*> tail) asyncOps ((->>) [])

    let traverseAsyncAParallel (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        List.foldBack (fun head tail -> cons <!> f head <&> tail) asyncOps ((->>) [])

    let sequenceAsyncM (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncM id asyncOps

    let sequenceAsyncA (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncA id asyncOps

    let sequenceAsyncAParallel (asyncOps: Async<'a> list) : Async<'a list> = traverseAsyncAParallel id asyncOps
