module FSharp.Prelude.Tests.ResultTests

open Expecto
open Prelude.Extensions

let test1 =
    test "Should sequence" {
        let sample = [ 1; 2; 3 ]

        let expected = Ok sample

        let input = List.map Result.singleton sample

        let actual = Result.sequence input

        Expect.equal actual expected "Should equal"
    }

[<Tests>]
let tests = testList "Result tests" [ test1 ]
