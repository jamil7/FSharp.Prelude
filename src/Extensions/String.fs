namespace Prelude.Extensions

[<RequireQualifiedAccess>]
module String =

    let append str1 str2 : string = str1 + str2

    let contains (value: string) (source: string) : bool = source.Contains value

    let isEmpty (str: string) : bool = str = ""

    let join (separator: string) (values: string list) : string = System.String.Join(separator, values)

    let padLeft (totalWidth: int) (str: string) = str.PadLeft totalWidth

    let padRight (totalWidth: int) (str: string) = str.PadRight totalWidth

    let replace (oldValue: string) (newValue: string) (source: string) =
        if String.length oldValue = 0 then
            source
        else
            source.Replace(oldValue, newValue)

    let reverse (str: string) : string =
        System.String(str.ToCharArray() |> Array.rev)

    let split (separator: string) (str: string) =
        str.Split(separator |> Seq.singleton |> Seq.toArray, System.StringSplitOptions.None)
        |> List.ofArray

    let lines (str: string) : string list = split System.Environment.NewLine str

    let startsWith (value: string) (source: string) : bool = source.StartsWith value

    let toLower (str: string) : string = str.ToLower()

    let toUpper (str: string) : string = str.ToUpper()

    let trim (str: string) : string = str.Trim()

    let trimRight (str: string) : string = str.TrimEnd()

    let trimStart (str: string) : string = str.TrimStart()
