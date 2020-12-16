namespace FSharp.Prelude

open System

[<RequireQualifiedAccess>]
module String =
    let isEmpty (str: string): bool = str = ""

    let reverse (str: string): string = String(str.ToCharArray() |> Array.rev)

    let replace (oldString: string) (newString: string) (source: string) =
        if String.length oldString = 0 then source else source.Replace(oldString, newString)

    let append str1 str2: String = str1 + str2

    let split (separator: string) (str: string) = str.Split(separator) |> List.ofArray

    let join (separator: string) (strings: string list): string = String.Join(separator, strings)

    let lines (str: string): string list = split Environment.NewLine str

    let toUpper (str: string): string = str.ToUpper()

    let toLower (str: string): string = str.ToLower()

    let padLeft (totalLength: int) (str: string) = str.PadLeft(totalLength)

    let padRight (totalLength: int) (str: string) = str.PadRight(totalLength)

    let trim (str: string): string = str.Trim()

    let trimStart (str: string): string = str.TrimStart()

    let trimRight (str: string): string = str.TrimEnd()
