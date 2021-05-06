module FSharp.Prelude.Tests.AsyncResultTests

open System.Threading
open System.Threading.Tasks
open FSharp.Prelude
open Expecto

[<Tests>]
let creationTests =
    let testSingleton text value =
        testAsync text {
            let! actual = AsyncResult.singleton value
            let expected = Ok value

            Expect.equal actual expected "should equal"
        }

    testList
        "Test creating new AsyncResult"
        [
            testList
                "singleton"
                [
                    testSingleton "should create an Ok AsyncResult given a string" "test"

                    testSingleton "should create an Ok AsyncResult given an int" 4

                    testSingleton "should create an Ok AsyncResult given a bool" true

                    testSingleton "should nest Ok in a Ok AsyncResult" (Ok 1)

                    testSingleton "should nest Error in a Ok AsyncResult" (Error "This is in an Ok")
                ]
        ]

[<Tests>]
let sequenceTests =
    testList
        "Sequence tests"
        [
            testAsync "should return values in same order as given tasks" {
                let expected = Ok [ 1; 2; 3 ]

                let input =
                    [
                        (AsyncResult.singleton 1)
                        (AsyncResult.singleton 2)
                        (AsyncResult.singleton 3)
                    ]

                let! actual = AsyncResult.sequence input
                Expect.equal actual expected "should equal"
            }
            testAsync "should prove example" {
                let fetchUser : int -> AsyncResult<int, 'err> = AsyncResult.singleton

                let userIds = [ 1; 2; 3 ]
                let expected = Ok userIds

                let! actual =
                    userIds
                    |> List.map fetchUser
                    |> AsyncResult.sequence

                Expect.equal actual expected "should equal"
            }
            testAsync "should execute async task in sequence" {
                let mutable orderRun = []

                let dummyAsync : int -> AsyncResult<int, string> =
                    fun i ->
                        AsyncResult.ofResult (Ok i)
                        |> AsyncResult.map
                            (fun j ->
                                orderRun <- List.append orderRun [ j ]
                                j)

                let input =
                    [
                        Async.Sleep 100
                        |> Async.bind (fun _ -> dummyAsync 1)
                        (dummyAsync 2)
                        (dummyAsync 3)
                    ]

                let expectedOkValue = [ 1; 2; 3 ]
                let! _actual = AsyncResult.sequence input
                Expect.equal orderRun expectedOkValue "Should be run in same order"
            }
        ]

[<Tests>]
let sequenceATests =
    testList
        "SequenceA tests"
        [
            testAsync "should return values in same order as given tasks" {
                let expected = Ok [ 1; 2; 3 ]

                let input =
                    [
                        (AsyncResult.singleton 1)
                        (AsyncResult.singleton 2)
                        (AsyncResult.singleton 3)
                    ]

                let! actual = AsyncResult.sequenceA input
                Expect.equal actual expected "should equal"
            }
            testAsync "should prove example" {
                let fetchUser : int -> AsyncResult<int, 'err> = AsyncResult.singleton

                let userIds = [ 1; 2; 3 ]
                let expected = Ok userIds

                let! actual =
                    userIds
                    |> List.map fetchUser
                    |> AsyncResult.sequenceA

                Expect.equal actual expected "should equal"
            }
            testAsync "should execute async task in sequence" {
                let mutable orderRun = []

                let dummyAsync : int -> AsyncResult<int, string> =
                    fun i ->
                        AsyncResult.ofResult (Ok i)
                        |> AsyncResult.map
                            (fun j ->
                                orderRun <- List.append orderRun [ j ]
                                j)

                let input =
                    [
                        Async.Sleep 100
                        |> Async.bind (fun _ -> dummyAsync 1)
                        (dummyAsync 2)
                        (dummyAsync 3)
                    ]

                let expectedOkValue = [ 1; 2; 3 ]
                let! _actual = AsyncResult.sequenceA input
                Expect.equal orderRun expectedOkValue "Should be run in same order"
            }
        ]

[<Tests>]
let traverseTests =
    testList
        "Traverse tests"
        [
            testAsync "should return values in same order as given tasks" {
                let input = [ 1; 2; 3 ]
                let expected = Ok [ 1; 2; 3 ]

                let! actual = AsyncResult.traverse AsyncResult.singleton input
                Expect.equal actual expected "should equal"
            }
            testAsync "should return map the AsyncResult values" {
                let transformer = ((+) 10) >> AsyncResult.singleton

                let input = [ 1; 2; 3 ]
                let expected = Ok [ 11; 12; 13 ]

                let! actual = AsyncResult.traverse transformer input
                Expect.equal actual expected "should equal"
            }
            testAsync "should make an early return if there is an Error" {
                let mutable currentItem = 0

                let transformer x =
                    currentItem <- x

                    if x = 2 then
                        AsyncResult.ofResult (Error "Skip next")
                    else
                        AsyncResult.singleton x

                let! _ = AsyncResult.traverse transformer [ 1; 2; 3 ]

                Expect.equal currentItem 2 "should equal"
            }
            testAsync "should prove example" {
                let fetchUser : int -> AsyncResult<int, 'err> = AsyncResult.singleton

                let userIds = [ 1; 2; 3 ]

                let expected = Ok userIds

                let! actual = AsyncResult.traverse fetchUser userIds

                Expect.equal actual expected "should equal"
            }
            testAsync "should execute async task in sequence" {
                let delay x =
                    Async.Sleep(x * 100)
                    |> Async.bind (fun _ -> AsyncResult.singleton x)

                let input = [ 0; 14; 1; 3 ]
                let expected = Ok input

                let! actual = AsyncResult.traverse delay input
                Expect.equal actual expected "Should be run in same order"
            }
        ]

[<Tests>]
let traverseParallelTests =
    testList
        "TraverseParallel tests"
        [
            testAsync "should return values in same order as given tasks" {
                let input = [ 1; 2; 3 ]
                let expected = Ok [ 1; 2; 3 ]

                let! actual = AsyncResult.traverseParallel AsyncResult.singleton input
                Expect.equal actual expected "should equal"
            }
            testAsync "should return map the AsyncResult values" {
                let transformer = ((+) 10) >> AsyncResult.singleton

                let input = [ 1; 2; 3 ]
                let expected = Ok [ 11; 12; 13 ]

                let! actual = AsyncResult.traverseParallel transformer input
                Expect.equal actual expected "should equal"
            }
            testAsync "should make an early return if there is an Error" {
                let mutable currentItem = 0

                let transformer x =
                    currentItem <- x

                    if x = 2 then
                        AsyncResult.ofResult (Error "Skip next")
                    else
                        AsyncResult.singleton x

                let! _ = AsyncResult.traverseParallel transformer [ 1; 2; 3 ]

                Expect.equal currentItem 2 "should equal"
            }
            testAsync "should prove example" {
                let fetchUser : int -> AsyncResult<int, 'err> = AsyncResult.singleton

                let userIds = [ 1; 2; 3 ]

                let expected = Ok userIds

                let! actual = AsyncResult.traverseParallel fetchUser userIds

                Expect.equal actual expected "should equal"
            }
            testAsync "should execute async task in sequence" {
                let delay x =
                    Async.Sleep(x * 100)
                    |> Async.bind (fun _ -> AsyncResult.singleton x)

                let input = [ 0; 14; 1; 3 ]
                let expected = Ok input

                let! actual = AsyncResult.traverseParallel delay input
                Expect.equal actual expected "Should be run in same order"
            }
        ]

[<Tests>]
let mapMTests =
    testList
        "MapM tests"
        [
            testAsync "should return values in same order as given tasks" {
                let input = [ 1; 2; 3 ]
                let expected = Ok [ 1; 2; 3 ]

                let! actual = AsyncResult.mapM AsyncResult.singleton input
                Expect.equal actual expected "should equal"
            }
            testAsync "should return map the AsyncResult values" {
                let transformer = ((+) 10) >> AsyncResult.singleton

                let input = [ 1; 2; 3 ]
                let expected = Ok [ 11; 12; 13 ]

                let! actual = AsyncResult.mapM transformer input
                Expect.equal actual expected "should equal"
            }
            testAsync "should make an early return if there is an Error" {
                let mutable currentItem = 0

                let transformer x =
                    currentItem <- x

                    if x = 2 then
                        AsyncResult.ofResult (Error "Skip next")
                    else
                        AsyncResult.singleton x

                let! _ = AsyncResult.mapM transformer [ 1; 2; 3 ]

                Expect.equal currentItem 2 "should equal"
            }
            testAsync "should prove example" {
                let fetchUser : int -> AsyncResult<int, 'err> = AsyncResult.singleton

                let userIds = [ 1; 2; 3 ]

                let expected = Ok userIds

                let! actual = AsyncResult.mapM fetchUser userIds

                Expect.equal actual expected "should equal"
            }
            testAsync "should execute async task in sequence" {
                let delay x =
                    Async.Sleep(x * 100)
                    |> Async.bind (fun _ -> AsyncResult.singleton x)

                let input = [ 0; 14; 1; 3 ]
                let expected = Ok input

                let! actual = AsyncResult.mapM delay input
                Expect.equal actual expected "Should be run in same order"
            }
        ]

[<Tests>]
let taskTests =
    testList
        "Task tests"
        [
            testList
                "ofUnitTask"
                [
                    testAsync "should convert from Task to AsyncResult" {
                        let source = new CancellationTokenSource()
                        let input : (unit -> Task) = fun () -> Task.Delay(0, source.Token)

                        let expectedValue = Ok()

                        let! actual = AsyncResult.ofUnitTask input

                        Expect.equal actual expectedValue "Should be equal"
                    }
                ]
            testList
                "ofTask"
                [
                    testAsync "should convert from Task<string> to AsyncResult" {
                        let input =
                            fun () -> Async.singleton "Hello" |> Async.StartAsTask

                        let expectedValue = Ok "Hello"

                        let! actual = AsyncResult.ofTask input

                        Expect.equal actual expectedValue "Should be equal"
                    }
                ]
        ]

[<Tests>]
let applyTest =
    testList
        "Test apply"
        [
            testAsync "should apply the value to function" {
                let xA = AsyncResult.singleton 42
                let fA = AsyncResult.singleton ((+) 10)

                let! actual = AsyncResult.apply fA xA

                let expectedValue = Ok 52

                Expect.equal actual expectedValue "Should be equal"
            }

            testAsync "should apply the value to all functions" {
                let xA = AsyncResult.singleton 42
                let fA1 = AsyncResult.singleton ((+) 10)
                let fA2 = AsyncResult.singleton (fun x -> x / 2)

                let! actual =
                    xA
                    |> AsyncResult.apply fA1
                    |> AsyncResult.apply fA2
                    |> AsyncResult.apply fA2

                let expectedValue = Ok 13

                Expect.equal actual expectedValue "Should be equal"
            }

            testAsync "should follow the law of Identity (apply id v = v)" {
                let v = AsyncResult.singleton 42
                let f = AsyncResult.singleton id

                let! actual = AsyncResult.apply f v
                let! expectedValue = v

                Expect.equal actual expectedValue "Should be equal"
            }
            testAsync "should follow the law of Homomorphism (apply fA xA = AR.of (f x)" {
                let x = 42
                let f = ((+) 10)

                let xA = AsyncResult.singleton x
                let fA = AsyncResult.singleton f

                let! actual = AsyncResult.apply fA xA
                let! expectedValue = AsyncResult.singleton (f x)

                Expect.equal actual expectedValue "Should be equal"
            }
            testAsync "should follow the law of Composition (xA |> apply fA1 |> apply fA2 = apply fA1 (apply fA2 xA))" {
                let fA1 = AsyncResult.singleton ((+) 10)
                let fA2 = AsyncResult.singleton ((-) 2)
                let xA = AsyncResult.singleton 42

                let! actual =
                    xA
                    |> AsyncResult.apply fA2
                    |> AsyncResult.apply fA1

                let! expectedValue = AsyncResult.apply fA1 (AsyncResult.apply fA2 xA)

                Expect.equal actual expectedValue "Should be equal"
            }
            testAsync "should wrap the Error value and result in an Error" {
                let fA1 = AsyncResult.singleton ((+) 10)
                let fA2 = AsyncResult.ofResult (Error 2)
                let xA = AsyncResult.singleton 42

                let! actual =
                    xA
                    |> AsyncResult.apply fA2
                    |> AsyncResult.apply fA1

                let expectedValue = Error 2

                Expect.equal actual expectedValue "Should be equal"
            }

            testAsync "should wrap the first Error value and result in an Error" {
                let fA1 = AsyncResult.ofResult (Error 1)
                let fA2 = AsyncResult.ofResult (Error 2)
                let xA = AsyncResult.singleton 42

                let! actual =
                    xA
                    |> AsyncResult.apply fA2
                    |> AsyncResult.apply fA1

                let expectedValue = Error 1

                Expect.equal actual expectedValue "Should be equal"
            }

            testAsync "should result in an Error if the value is an Error" {
                let fA1 = AsyncResult.singleton ((+) 10)
                let fA2 = AsyncResult.singleton ((-) 2)
                let xA = AsyncResult.ofResult (Error 42)

                let! actual =
                    xA
                    |> AsyncResult.apply fA2
                    |> AsyncResult.apply fA1

                let expectedValue = Error 42

                Expect.equal actual expectedValue "Should be equal"
            }

        ]

[<Tests>]
let mapTests =
    testList
        "Test map functions"
        [
            testList
                "map"
                [
                    testAsync "should change the value in an AsyncResult" {
                        let input = AsyncResult.singleton 3

                        let expectedValue = Ok 4

                        let! actual = AsyncResult.map ((+) 1) input

                        Expect.equal actual expectedValue "Should be equal"
                    }
                    testAsync "should NOT change the value in an Error AsyncResult" {
                        let input = AsyncResult.ofResult (Error 3)

                        let expectedValue = Error 3

                        let! actual = AsyncResult.map ((+) 1) input

                        Expect.equal actual expectedValue "Should be equal"
                    }
                ]

            testList
                "map2"
                [
                    testAsync "should map over the value from two AsyncResult" {
                        let input1 = AsyncResult.singleton 3
                        let input2 = AsyncResult.singleton 7

                        let expectedValue = Ok 10

                        let! actual = AsyncResult.map2 (+) input1 input2

                        Expect.equal actual expectedValue "Should be equal"
                    }

                    testAsync "should fail if the first AsyncResult is an error" {
                        let input1 = AsyncResult.ofResult (Error "Not Ok")
                        let input2 = AsyncResult.singleton 3

                        let expectedValue = Error "Not Ok"

                        let! actual = AsyncResult.map2 (+) input1 input2

                        Expect.equal actual expectedValue "Should be equal"
                    }

                    testAsync "should fail if the second AsyncResult is an error" {
                        let input1 = AsyncResult.singleton 3

                        let input2 =
                            AsyncResult.ofResult (Error "Not Ok either")

                        let expectedValue = Error "Not Ok either"

                        let! actual = AsyncResult.map2 (+) input1 input2

                        Expect.equal actual expectedValue "Should be equal"
                    }

                    testAsync "should pass arguments in order" {
                        let input1 = AsyncResult.singleton 3
                        let input2 = AsyncResult.singleton 7

                        let expected = Ok(3, 7)

                        let! actual = AsyncResult.map2 (fun a b -> (a, b)) input1 input2

                        Expect.equal actual expected "Should be equal"

                    }
                ]

            testList
                "andMap"
                [
                    testAsync "should apply AsyncResult value to AsyncResult function" {
                        let fA = AsyncResult.singleton ((+) 10)
                        let xA = AsyncResult.singleton 20

                        let expected = Ok 30

                        let! actual = AsyncResult.andMap xA fA

                        Expect.equal actual expected "Should be equal"
                    }
                    testAsync "should be pipeable" {
                        let fA =
                            AsyncResult.singleton (fun a b c -> a + b + c)

                        let xA = AsyncResult.singleton 10
                        let yA = AsyncResult.singleton 20
                        let zA = AsyncResult.singleton 30

                        let expected = Ok 60

                        let! actual =
                            fA
                            |> AsyncResult.andMap xA
                            |> AsyncResult.andMap yA
                            |> AsyncResult.andMap zA

                        Expect.equal actual expected "Should be equal"
                    }
                    testAsync "should fail if there is an Error" {
                        let fA =
                            AsyncResult.singleton (fun a b c -> a + b + c)

                        let xA = AsyncResult.singleton 10

                        let yA =
                            AsyncResult.ofResult (Error "Oh no an error")

                        let zA = AsyncResult.singleton 30

                        let expected = Error "Oh no an error"

                        let! actual =
                            fA
                            |> AsyncResult.andMap xA
                            |> AsyncResult.andMap yA
                            |> AsyncResult.andMap zA

                        Expect.equal actual expected "Should be equal"
                    }
                ]
        ]

[<Tests>]
let bindTests =
    testList
        "Test bind functions"
        [
            testList
                "bind"
                [
                    testAsync "should change the value in an AsyncResult" {
                        let input = AsyncResult.singleton 3

                        let f = ((+) 1 >> AsyncResult.singleton)

                        let expectedValue = Ok 4

                        let! actual = AsyncResult.bind f input

                        Expect.equal actual expectedValue "Should be equal"
                    }

                    testAsync "should NOT change the value in an Error AsyncResult" {
                        let input = AsyncResult.ofResult (Error 3)

                        let f = (+) 1 >> Ok >> AsyncResult.ofResult

                        let expectedValue = Error 3

                        let! actual = AsyncResult.bind f input

                        Expect.equal actual expectedValue "Should be equal"
                    }
                ]
        ]
