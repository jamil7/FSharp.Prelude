namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    let cons = cons

    // Options operations

    let mapOptionM (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.mapM f options

    let traverseOption (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverse f options

    let sequenceOption (options: Option<'a> list) : Option<'a list> = Option.sequence options

    let sequenceOptionA (options: Option<'a> list) : Option<'a list> = Option.sequenceA options


    // Results Operations

    let mapResultM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.mapM f results

    let traverseResult (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.traverse f results

    let sequenceResult (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequence results

    let sequenceResultA (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceA results


    // Asyncs Operations

    let mapAsyncM (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> = Async.mapM f asyncOps

    let traverseAsync (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> = Async.traverse f asyncOps

    let traverseAsyncParallel (f: 'a -> Async<'b>) (asyncOps: 'a list) : Async<'b list> =
        Async.traverseParallel f asyncOps

    let sequenceAsync (asyncOps: Async<'a> list) : Async<'a list> = Async.sequence asyncOps

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
