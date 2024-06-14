#r "nuget: FSharp.Data"

open FSharp.Data

[<Measure>]
type minute

let datetime (d: System.DateTime) (t: System.TimeSpan) = d.Add(t)

type Feeding =
    {
        s: System.DateTime
        e: System.DateTime
    }

    member z.duration = (z.e - z.s).TotalMinutes

let feeding s e = { s = s; e = e }

let fromRow da ta db tb =
    feeding (datetime da ta) (datetime db tb)

let interduration (later: System.DateTime) (earlier: System.DateTime) =
    (later - earlier).TotalMinutes * 1.0<minute>

type FeedingRecords = CsvProvider<"./Feeding Records.csv">
let fdb = FeedingRecords.Load("./Feeding Records.csv")

let feedings =
    [
        for row in fdb.Rows do
            fromRow row.StartDate row.Start row.EndDate row.End
    ]

type SessionState =
    private
        {
            lastEnd: System.DateTime
            currentSession: option<System.DateTime * Feeding list>
            sessions: Map<System.DateTime, Feeding list>
        }

    override z.ToString() =
        [
            $"Feeding Sessions ({Map.count z.sessions}):"
            yield!
                z.sessions
                |> Map.map (fun k v ->
                    let date = $"%04d{k.Year}-%02d{k.Month}-%02d{k.Day} %02d{k.Hour}:%02d{k.Minute}"

                    $"%s{date}({v |> List.length}): ["
                    + (v
                       |> List.map (fun f ->
                           $"%02d{f.s.TimeOfDay.Hours}:%02d{f.s.TimeOfDay.Minutes}(%02d{int f.duration})"
                       )
                       |> String.concat "; ")
                    + "]"
                )
                |> Map.values
        ]

        |> String.concat System.Environment.NewLine

let initialState () =
    {
        lastEnd = System.DateTime.MinValue
        currentSession = None
        sessions = Map.empty
    }

let newSession (s: SessionState) (f: Feeding) =
    { s with
        sessions =
            match s.currentSession with
            | Some(k, v) -> Map.add k v s.sessions
            | _ -> s.sessions
        currentSession = Some(f.s, [ f ])
        lastEnd = f.e
    }

let addToSession (s: SessionState) (f: Feeding) =
    { s with
        currentSession =
            match s.currentSession with
            | Some(k, v) -> Some(k, [ yield! v; f ])
            | None -> Some(f.s, [ f ])
    }

let sessions (s: SessionState) = s.sessions

feedings
|> List.sortBy (fun f -> f.s)
|> List.fold
    (fun s t ->
        let inter = interduration t.s s.lastEnd

        if inter > 30.<minute> then
            newSession s t
        else
            addToSession s t
    )
    (initialState ())
|> printfn "%O"
