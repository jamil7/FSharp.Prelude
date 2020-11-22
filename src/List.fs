namespace FSharp.Prelude

module List =
    let cons (head: 'a) (tail: 'a list): 'a list = head :: tail

    let foldl (folder: 'a -> 'b -> 'a) (state: 'a) (list: 'b list): 'a = List.fold folder state list

    let foldr (folder: 'a -> 'b -> 'b) (state: 'b) (list: 'a list): 'b = List.foldBack folder list state
