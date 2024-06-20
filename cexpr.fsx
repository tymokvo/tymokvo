module Eventually =
    type Eventually<'t> =
        | Done of 't
        | NotYetDone of (unit -> Eventually<'t>)

    let rec bind func e =
        match e with
        | Done v -> func v
        | NotYetDone work -> NotYetDone(fun () -> bind func (work ()))

    let result value = Done value

    let rec catch e =
        match e with
        | Done v -> result (Ok v)
        | NotYetDone work ->
            NotYetDone(fun () ->
                ()
                |> (fun () ->
                    try
                        Ok(work ())
                    with ex ->
                        Error ex
                )
                |> function
                    | Ok v -> catch v
                    | Error ex -> result (Error ex)
            )

    let delay func = NotYetDone(fun () -> func ())

    let step e =
        match e with
        | Done _ -> e
        | NotYetDone func -> func ()

    let tryFinally e fin =
        e
        |> catch
        |> bind (fun res ->
            fin ()

            match res with
            | Ok v -> result v
            | Error ex -> raise ex
        )

    let tryWith ex handler =
        catch ex
        |> bind (
            function
            | Ok v -> result v
            | Error ex -> handler ex
        )

    let rec whileLoop pred body =
        ()
        |> pred
        |> function
            | true -> body |> bind (fun _ -> whileLoop pred body)
            | _ -> result ()

    let combine ea eb = ea |> bind (fun () -> eb)

    let using (thing: #System.IDisposable) f =
        tryFinally (f thing) (fun () -> thing.Dispose())

    let forLoop (coll: seq<_>) f =
        let ie = coll.GetEnumerator()

        tryFinally
            (whileLoop (fun () -> ie.MoveNext()) (delay (fun () -> let value = ie.Current in f value)))
            (fun () -> ie.Dispose())

    type EventuallyBuilder() =
        member z.Bind(c, f) = bind f c
        member z.Return(v) = result v
        member z.ReturnFrom(v) = v
        member z.Combine(ea, eb) = combine ea eb
        member z.Delay(f) = delay f
        member z.Zero() = result ()
        member z.TryWith(e, h) = tryWith e h
        member z.TryFinally(e, fin) = tryFinally e fin
        member z.For(coll, f) = forLoop coll f
        member z.Using(r, e) = using r e

    let eventually = new EventuallyBuilder()

    let comp =
        eventually {
            for x in 1..2 do
                printfn $"x = %d{x}"

            return 3 + 4
        }

module Logging =
    type LoggingBuilder() =
        let log = printfn "%A"

        member z.Bind(x, f) =
            log x
            f x

        member z.Return(x) = x

        member z.Zero() = ()

    let logging = new LoggingBuilder()
