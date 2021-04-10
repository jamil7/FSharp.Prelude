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

    let traverseAsyncResultM (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.traverseM f asyncResults

    let traverseAsyncResultA (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.traverseA f asyncResults

    let traverseAsyncResultAParallel (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.traverseAParallel f asyncResults

    let sequenceAsyncResultM (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceM asyncResults

    let sequenceAsyncResultA (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceA asyncResults

    let sequenceAsyncResultAParallel (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceAParallel asyncResults


    // AsyncOptions Operations

    let traverseAsyncOptionM (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.traverseM f asyncOptions

    let traverseAsyncOptionA (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.traverseA f asyncOptions

    let traverseAsyncOptionAParallel (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.traverseAParallel f asyncOptions

    let sequenceAsyncOptionM (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequenceM asyncOptions

    let sequenceAsyncOptionA (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequenceA asyncOptions

    let sequenceAsyncOptionAParallel (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequenceAParallel asyncOptions


    // AsyncResultOptions Operations

    let traverseAsyncResultOptionM
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        AsyncResultOption.traverseM f asyncResultOptions

    let traverseAsyncResultOptionA
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        AsyncResultOption.traverseA f asyncResultOptions

    let traverseAsyncResultOptionAParallel
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        AsyncResultOption.traverseAParallel f asyncResultOptions

    let sequenceAsyncResultOptionM
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequenceM asyncResultOptions

    let sequenceAsyncResultOptionA
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequenceA asyncResultOptions

    let sequenceAsyncResultOptionAParallel
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequenceAParallel asyncResultOptions
