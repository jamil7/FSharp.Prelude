namespace FSharp.Prelude.Operators.Option

[<AutoOpen>]
module OptionOperators =
    let (!>) (value: 'a) : 'a option = Some value

    /// Infix map operator.
    let inline (<!>) (f: 'a -> 'b) (option: 'a option) : 'b option = Option.map f option

    /// Infix apply operator.
    let inline (<*>) (f: ('a -> 'b) option) (option: 'a option) : 'b option =
        match f, option with
        | Some f', Some something -> Some(f' something)
        | None, _ -> None
        | _, None -> None

    /// Infix bind operator.
    let inline (>>=) (option: 'a option) (f: 'a -> 'b option) : 'b option = Option.bind f option

    let inline (>=>) (f: 'a -> 'b option) (g: 'b -> 'c option) : 'a -> 'c option =
        fun x ->
            match f x with
            | Some thing -> g thing
            | None -> None

    /// Infix alternative operator.
    let inline (<|>) (option1: 'a option) (option2: 'a option) : 'a option =
        match option1, option2 with
        | None, right -> right
        | left, _ -> left


namespace FSharp.Prelude

open FSharp.Prelude.Operators.Option

[<RequireQualifiedAccess>]
module List =

    let traverseOptionM (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> =
        List.foldBack
            (fun head tail ->
                f head
                >>= (fun head' -> tail >>= (fun tail' -> !>(List.cons head' tail'))))
            options
            (!> [])

    let traverseOptionA (f: 'a -> Option<'b>) (options: 'a list) : Option<'b list> =
        List.foldBack (fun head tail -> List.cons <!> f head <*> tail) options (!> [])

    let sequenceOptionM (asyncOps: Option<'a> list) : Option<'a list> = traverseOptionM id asyncOps

    let sequenceOptionA (options: Option<'a> list) : Option<'a list> = traverseOptionA id options


[<RequireQualifiedAccess>]
module Option =

    let singleton (value: 'a) : 'a option = !>value

    let apply (f: ('a -> 'b) option) (option: 'a option) : 'b option = f <*> option

    let alternative (option1: 'a option) (option2: 'a option) : 'a option = option1 <|> option2

    let andMap (option: 'a option) (f: ('a -> 'b) option) : 'b option = Option.map2 (|>) option f

    let compose (f: 'a -> 'b option) (g: 'b -> 'c option) : 'a -> 'c option = f >=> g

    let sequence (options: 'a option list) : 'a list option = List.traverseOptionM id options

    let zip (option1: 'a option) (option2: 'b option) : ('a * 'b) option =
        (fun a b -> a, b) <!> option1 <*> option2

    let ofResult (result: Result<'a, 'b>) : 'a option =
        match result with
        | Ok ok -> Some ok
        | Error _ -> None

    let ofChoice (choice: Choice<'a, 'b>) : 'a option =
        match choice with
        | Choice1Of2 left -> Some left
        | Choice2Of2 _ -> None

[<AutoOpen>]
module OptionCE =

    type OptionBuilder() =
        member _.Return(value) : 'a option = Option.singleton value

        member _.ReturnFrom(option: 'a option) : 'a option = option

        member _.Zero() : unit option = Option.singleton ()

        member _.Bind(option: 'a option, f: 'a -> 'b option) : 'b option = Option.bind f option

        member _.Delay(f: unit -> 'a option) : unit -> 'a option = f

        member _.Run(f: unit -> 'a option) : 'a option = f ()

        member _.Combine(option: 'a option, f: 'a -> 'b option) : 'b Option = Option.bind f option

        member _.TryWith(f: unit -> 'a option, g: exn -> 'a option) : 'a option =
            try
                f ()
            with exn -> g exn

        member _.TryFinally(f: unit -> 'a option, g: unit -> unit) : 'a option =
            try
                f ()
            finally
                g ()

        member _.Using(m: 'a :> System.IDisposable, f: 'a -> 'a option) : 'a option =
            try
                (fun () -> f m) ()
            finally
                (fun () ->
                    if not (obj.ReferenceEquals(m, null)) then
                        m.Dispose())
                    ()

        member this.While(f: unit -> bool, g: unit -> Option<unit>) : Option<unit> =
            if not (f ()) then
                this.Zero()
            else
                g () |> Option.bind (fun () -> this.While(f, g))

        member _.BindReturn(option: 'a option, f: 'a -> 'b) : 'b option = Option.map f option

        member _.MergeSources(option1: 'a option, option2: 'b option) = Option.zip option1 option2

    let option = OptionBuilder()
