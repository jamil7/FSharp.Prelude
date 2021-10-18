module Prelude.Tests.DependencyManagement.ReaderAsyncResult

open Expecto
open Expecto.Flip
open Prelude.FDI
open Prelude.Extensions

type Env1 = { apiKey: string; db: string }
type Env2 = { greeting: string }

type SuperEnv =
    {
        apiKey: string
        db: string
        greeting: string
    }

let env = { apiKey = "some_key"; db = "postgres" }

let connectionString: ReaderAsyncResult<Env1, string, string> =
    readerAsyncResult {
        let! r = ReaderAsyncResult.ask
        return $"{r.apiKey} {r.db}"
    }

let greeting name : ReaderAsyncResult<Env2, string, string> =
    readerAsyncResult {
        let! r = ReaderAsyncResult.ask
        return $"{r.greeting} {name}"
    }

let connectiveGreeting: ReaderAsyncResult<SuperEnv, string, string> =
    readerAsyncResult {
        let! r = ReaderAsyncResult.ask

        let! c =
            connectionString
            |> ReaderAsyncResult.withReader (always { apiKey = r.apiKey; db = r.db })

        return c
    }


let test1 =
    test "some test" {
        connectionString
        |> ReaderAsyncResult.run env
        |> Expect.wantOk "should be ok"
        |> Expect.equal "should equal" $"{env.apiKey} {env.db}"
    }

[<Tests>]
let tests = testList "Ad-hoc test" [ test1 ]
