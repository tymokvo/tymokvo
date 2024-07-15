#r "nuget: FSharp.Data"

open FSharp.Data

[<Measure>]
type minute

let datetime (d: System.DateTime) (t: System.TimeSpan) = d.Add(t)

type Feeding =
    {
        s: System.DateTime
        e: System.DateTime
        src: string
    }

    member z.duration = (z.e - z.s).TotalMinutes * 1.0<minute>

    member z.source =
        match z.src with
        | "right" -> "R"
        | "left" -> "L"
        | "bottle" -> "B"
        | _ -> "?"

let feeding s e src = { s = s; e = e; src = src }

let fromRow da ta db tb src =
    feeding (datetime da ta) (datetime db tb) src

let interduration (later: System.DateTime) (earlier: System.DateTime) =
    (later - earlier).TotalMinutes * 1.0<minute>

type FeedingRecords = CsvProvider<"./Feeding Records.csv">
let fdb = FeedingRecords.Load("./Feeding Records.csv")

let feedings =
    [
        for row in fdb.Rows do
            fromRow row.StartDate row.Start row.EndDate row.End row.Source
    ]

let (|/) a b = b / a

let averageDuration (fs: Feeding seq) =
    fs |> Seq.sumBy (fun f -> f.duration) |> (|/) (fs |> Seq.length |> float)

type SessionState =
    private
        {
            lastEnd: System.DateTime
            currentSession: option<System.DateTime * Feeding list>
            sessions: Map<System.DateTime, Feeding list>
        }

    override z.ToString() =
        [
            $"Feeding Sessions:"
            yield!
                z.sessions
                |> Map.toSeq
                |> Seq.sortByDescending (fun (k, _) -> k.Date)
                |> Seq.groupBy (fun (k, _) -> k.Date)
                |> Seq.collect (fun (date, sessions) ->
                    let avg = sessions |> Seq.collect snd |> averageDuration

                    let header =
                        $"{date.DayOfWeek}: {date.Year}-%02d{date.Month}-%02d{date.Day}: Avg: %0.1f{avg} min"

                    seq {
                        String.replicate (String.length header) "-"
                        header

                        yield!
                            sessions
                            |> Seq.mapi (fun idx (_, fs) ->
                                $"%02d{idx + 1}: ["
                                + (fs
                                   |> Seq.map (fun f ->
                                       $"%02d{f.s.Hour}:%02d{f.s.Minute}({f.source})(%02d{int f.duration})"
                                   )
                                   |> String.concat "; ")
                                + "]"
                            )
                    }
                )
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
