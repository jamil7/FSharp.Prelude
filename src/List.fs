namespace FSharp.Prelude

[<RequireQualifiedAccess>]
module List =

    let cons head tail = head :: tail

    // Options operations

    let traverseOptionM (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverseM f options

    let traverseOptionA (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> = Option.traverseA f options

    let sequenceOptionM (options: Option<'a> list) : Option<'a list> = Option.sequenceM options

    let sequenceOptionA (options: Option<'a> list) : Option<'a list> = Option.traverseA id options
