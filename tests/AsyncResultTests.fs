module Tests

open Expecto
open FSharp.Prelude
open FSharp.Prelude

[<Tests>]
let tests =
  testList "AsyncResult tests" [
    
    testAsync "Should sequence" {
       let sample = [1 ; 2; 3]
       
       let expected = Ok sample
       
       let input = List.map AsyncResult.singleton sample
       
       let! actual = AsyncResult.sequence input
       
       List.
       
       Expect.equal actual expected "Should equal"
    }
  ]
