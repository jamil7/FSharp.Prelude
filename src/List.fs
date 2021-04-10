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

    let internal traverseResultM (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        Result.traverseM f results

    let internal traverseResultA (f: 'a -> Result<'b, 'e>) (results: 'a list) : Result<'b list, 'e> =
        Result.traverseA f results

    let internal sequenceResultM (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceM results

    let internal sequenceResultA (results: Result<'a, 'e> list) : Result<'a list, 'e> = Result.sequenceA results
