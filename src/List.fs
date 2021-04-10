namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    let cons head tail = head :: tail

    // Options operations

    let traverseOptionM (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverseM f options

    let traverseOptionA (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverseA f options

    let sequenceOptionM (options: Option<'a> list) : Option<'a list> = Option.sequenceM options

    let sequenceOptionA (options: Option<'a> list) : Option<'a list> = Option.traverseA id options


    // Results Operations

    let traverseResultM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.traverseM f results

    let traverseResultA (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.traverseA f results

    let sequenceResultM (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceM results

    let sequenceResultA (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceA results


    // Asyncs Operations

    let traverseAsyncM (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> = Async.traverseM f asyncOps

    let traverseAsyncA (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> = Async.traverseA f asyncOps

    let traverseAsyncAParallel (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        Async.traverseAParallel f asyncOps

    let sequenceAsyncM (asyncOps: Async<'a> list) : Async<'a list> = Async.sequenceM asyncOps

    let sequenceAsyncA (asyncOps: Async<'a> list) : Async<'a list> = Async.sequenceA asyncOps

    let sequenceAsyncAParallel (asyncOps: Async<'a> list) : Async<'a list> = Async.sequenceAParallel asyncOps

    // AsyncResults Operations

    let internal traverseAsyncResultM
        (f: 'a -> AsyncResult<'b, 'e>)
        (asyncResults: 'a list)
        : AsyncResult<'b list, 'e> =
        AsyncResult.traverseM f asyncResults

    let internal traverseAsyncResultA
        (f: 'a -> AsyncResult<'b, 'e>)
        (asyncResults: 'a list)
        : AsyncResult<'b list, 'e> =
        AsyncResult.traverseA f asyncResults

    let internal traverseAsyncResultAParallel
        (f: 'a -> AsyncResult<'b, 'e>)
        (asyncResults: 'a list)
        : AsyncResult<'b list, 'e> =
        AsyncResult.traverseAParallel f asyncResults

    let internal sequenceAsyncResultM (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceM asyncResults

    let internal sequenceAsyncResultA (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceA asyncResults

    let internal sequenceAsyncResultAParallel (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceAParallel asyncResults


    // AsyncOptions Operations
    
    