namespace Prelude

[<RequireQualifiedAccess>]
module List =

    let cons (head: 'a) (tail: 'a list) : 'a list = cons head tail
