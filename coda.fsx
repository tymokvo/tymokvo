#r "nuget: FsHttp"

open FsHttp

module Path =
    let basePath = "https://coda.io/apis/v1"

    let path segments =
        seq {
            basePath
            yield! segments
        }
        |> String.concat "/"

    let whoAmI = path ["whoami"]

    let docs = path ["docs"]

    let tables doc = path ["docs"; doc; "tables"]

    let table doc table = path ["docs"; doc; "tables"; table]

    let rows doc table = path ["docs"; doc; "tables"; table; "rows"]

let token = (System.Environment.GetEnvironmentVariable("CODA_API_KEY"))

let getTables token (doc: string) =
    http {
        AuthorizationBearer token
        GET (Path.tables doc)
    }
    |> Request.send

let getTable token (doc: string) (id: string) =
    http {
        AuthorizationBearer token
        GET (Path.table doc id)
    }
    |> Request.send

let getDocs token =
    http {
        AuthorizationBearer token
        GET Path.docs
    }
    |> Request.send

let whoAmI token =
    http {
        AuthorizationBearer token
        GET Path.whoAmI
    }
    |> Request.send

let getRows token doc table =
    http {
        AuthorizationBearer token
        GET (Path.rows doc table)
    }
    |> Request.send

let main () =
    getTable token "86Sbdr2Lr-" "grid-I7-NpmL1Sp" |> Response.toJson |> printfn "%A"

main ()
