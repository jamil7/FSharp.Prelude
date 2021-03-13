namespace FSharp.Prelude

open System

[<RequireQualifiedAccess>]
module String =
    let contains (value: string) (source: string): bool = source.Contains value

    let isEmpty (str: string): bool = str = ""

    let reverse (str: string): string = String(str.ToCharArray() |> Array.rev)

    let replace (oldValue: string) (newValue: string) (source: string) =
        if String.length oldValue = 0 then source else source.Replace(oldValue, newValue)

    let append str1 str2: string = str1 + str2

    let split (separator: string) (str: string) = str.Split(separator) |> List.ofArray

    let join (separator: string) (values: string list): string = String.Join(separator, values)

    let lines (str: string): string list = split Environment.NewLine str

    let toUpper (str: string): string = str.ToUpper()

    let toLower (str: string): string = str.ToLower()

    let padLeft (totalWidth: int) (str: string) = str.PadLeft totalWidth

    let padRight (totalWidth: int) (str: string) = str.PadRight totalWidth

    let trim (str: string): string = str.Trim()

    let trimStart (str: string): string = str.TrimStart()

    let trimRight (str: string): string = str.TrimEnd()

    let startsWith (value: string) (source: string): bool = source.StartsWith value
