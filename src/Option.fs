namespace FSharp.Prelude.Operators

[<AutoOpen>]
module OptionOperators =
    let (<!>) (f: 'a -> 'b) (option: 'a option): 'b option = Option.map f option

    let (<*>) (f: ('a -> 'b) option) (option: 'a option): 'b option =
        match f, option with
        | Some f', Some something -> Some(f' something)
        | None, _ -> None
        | _, None -> None

    let (>>=) (f: 'a -> 'b option) (option: 'a option): 'b option = Option.bind f option

    let (<|>) (option1: 'a option) (option2: 'a option): 'a option =
        match option1, option2 with
        | None, right -> right
        | left, _ -> left

namespace FSharp.Prelude

open FSharp.Prelude.Operators

[<RequireQualifiedAccess>]
module Option =
    let singleton value = Some value

    let apply (f: ('a -> 'b) option) (option: 'a option): 'b option = f <*> option

    let alternative (option1: 'a option) (option2: 'a option): 'a option = option1 <|> option2

    let andMap (option: 'a option) (f: ('a -> 'b) option): 'b option = Option.map2 (|>) option f

    let sequence (options: 'a option list): 'a list option =
        List.foldr (fun head tail -> List.cons <!> head <*> tail) (singleton []) options

    let zip option1 option2 =
        (fun a b -> a, b) <!> option1 <*> option2

    let ofResult (result: Result<'a, 'b>): 'a option =
        match result with
        | Ok ok -> Some ok
        | Error _ -> None

    let ofChoice (choice: Choice<'a, 'b>): 'a option =
        match choice with
        | Choice1Of2 left -> Some left
        | Choice2Of2 _ -> None

[<AutoOpen>]
module OptionCE =
    type OptionBuilder() =
        member _.Return(x) = Option.singleton x

        member _.ReturnFrom(m: 'a option) = m

        member _.Zero() = Option.singleton ()

        member _.Bind(m, f) = f >>= m

        member _.Delay(f: unit -> 'a option) = f

        member _.Run(f: unit -> 'a option) = f ()

        member _.Combine(m: 'a option, f: 'a -> 'b option) = f >>= m

        member _.TryWith(f: unit -> 'a option, g: exn -> 'a option) =
            try
                f ()
            with e -> g e

        member _.TryFinally(f: unit -> 'a option, g: unit -> unit) =
            try
                f ()
            finally
                g ()

        member _.Using(m: 'a :> System.IDisposable, f: 'a -> 'a option) =
            try
                (fun () -> f m) ()
            finally
                (fun () -> if not (obj.ReferenceEquals(m, null)) then m.Dispose()) ()

        member _.BindReturn(m, f) = f <!> m

        member _.MergeSources(m1, m2) = Option.zip m1 m2
