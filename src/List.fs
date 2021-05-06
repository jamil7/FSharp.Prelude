namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    let cons = cons

    // Options operations

    let mapOptionM (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.mapM f options

    let traverseOption (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverse f options

    let sequenceOption (options: Option<'a> list) : Option<'a list> = Option.sequence options

    let sequenceOptionA (options: Option<'a> list) : Option<'a list> = Option.sequenceA options

    // Results operations

    let mapResultM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.mapM f results

    let traverseResult (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> = Result.traverse f results

    let sequenceResult (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequence results

    let sequenceResultA (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceA results

    // AsyncResults operations

    let mapAsyncResultM (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.mapM f asyncResults

    let traverseAsyncResult (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.traverse f asyncResults

    let traverseAsyncResultParallel (f: 'a -> AsyncResult<'b, 'e>) (asyncResults: 'a list) : AsyncResult<'b list, 'e> =
        AsyncResult.traverseParallel f asyncResults

    let sequenceAsyncResult (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequence asyncResults
 
    let sequenceAsyncResultA (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceA asyncResults

    let sequenceAsyncResultAParallel (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
        AsyncResult.sequenceAParallel asyncResults

    // AsyncOptions operations

    let mapAsyncOptionM (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.mapM f asyncOptions

    let traverseAsyncOption (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.traverse f asyncOptions

    let traverseAsyncOptionParallel (f: 'a -> AsyncOption<'b>) (asyncOptions: 'a list) : AsyncOption<'b list> =
        AsyncOption.traverseParallel f asyncOptions

    let sequenceAsyncOption (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequence asyncOptions

    let sequenceAsyncOptionA (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequenceA asyncOptions

    let sequenceAsyncOptionAParallel (asyncOptions: AsyncOption<'a> list) : AsyncOption<'a list> =
        AsyncOption.sequenceAParallel asyncOptions

    // AsyncResultOptions operations

    let mapAsyncResultOptionM
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        AsyncResultOption.mapM f asyncResultOptions

    let traverseAsyncResultOption
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> =
        AsyncResultOption.traverse f asyncResultOptions

    let traverseAsyncResultOptionParallel
        (f: 'a -> AsyncResultOption<'b, 'e>)
        (asyncResultOptions: 'a list)
        : AsyncResultOption<'b list, 'e> = 
        AsyncResultOption.traverseParallel f asyncResultOptions

    let sequenceAsyncResultOption
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequence asyncResultOptions

    let sequenceAsyncResultOptionA
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequenceA asyncResultOptions

    let sequenceAsyncResultOptionAParallel
        (asyncResultOptions: AsyncResultOption<'a, 'e> list)
        : AsyncResultOption<'a list, 'e> =
        AsyncResultOption.sequenceAParallel asyncResultOptions
