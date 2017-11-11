module TradingBot.Simulator

open TradingBot
open Bot
open Csv
open System.IO
open System


type Data =
    {
        [<Column(Index=0)>] Date : string
        [<Column(Index=1)>] Open : float
        [<Column(Index=2)>] High : float
        [<Column(Index=3)>] Low : float
        [<Column(Index=4)>] Close : float
        [<Column(Index=5)>] Volume : float
    }

type DataEx =
    {
        [<Column(Index=0)>] Date : string
        [<Column(Index=1)>] Time : string
        [<Column(Index=2)>] Open : float
        [<Column(Index=3)>] High : float
        [<Column(Index=4)>] Low : float
        [<Column(Index=5)>] Close : float
        [<Column(Index=6)>] Volume : float
        [<Column(Index=7)>] OpenInt : int
    }

type CSVData =
    | Normal of Data
    | Extended of DataEx


let Read<'a> path f =
    let typeConverter = function
                        | t when t = typeof<float>  -> Double.Parse >> box
                        | t when t = typeof<int>    -> Int32.Parse >> box
                        | t when t = typeof<string> -> fun (s:string) -> box s
                        | t when t = typeof<bool>   -> Boolean.Parse >> box
                        | t -> failwithf "Unknown type %A" t
    (CsvReader<'a> typeConverter).ReadFile(path, ',', true)
    |> Seq.map f
    |> Seq.toArray

let ReadCSV path =
    try Read<Data> path Normal
    with _ -> Read<DataEx> path Extended

let CreateFakeCSV path (rate : ExchangeRate) =
    let wr = File.CreateText path

    wr.WriteLine "Date,Open,High,Low,Close,Volume"
    
    iterate (fun x -> x + 1UL) 0UL
    |> Seq.map (fun i -> (i, match rate <| uint64 i with
                             | Some r -> r
                             | None -> nan))
    |> Seq.takeWhile (fun (_, r) -> r <> nan)
    |> Seq.iter (fun (i, v) -> wr.WriteLine(sprintf "0-%d,%.2f,%.2f,%.2f,%.2f,1000000" i v v v v))

    wr.Dispose()

let SimulateFake bot (rate : ExchangeRate) risk budget lookback =
    let print (t, s : State) = printf "%5d:  %12.2f EUR   %12.2f EUR   %12d    %A\n" t (match s.ExchangeRate with
                                                                                        | Some r -> r
                                                                                        | None -> nan) (s.Budget) (s.AssetCount) (s.LastAction)
    let hist = TradeHistory bot rate budget risk lookback
               |> Seq.toArray
    let b0 = (snd hist.[0]).Budget
    let bn = (snd hist.[hist.Length - 1]).Budget
    
    printf " time:              rate             budget         assets  action\n"
    Array.iter print hist
    printf "diff:\n%.2f EUR ---> %.2f EUR   (%.2f EUR)" b0 bn (bn - b0)

    hist

let SimulateCSV bot csv risk budget lookback =
    let data = ReadCSV csv
    //let datf = [for d in data -> [d.Open; (d.High + d.Low) / 2.0; d.Close]]
    //           |> List.concat
    //           |> List.toArray
    let datf = Array.map (function
                          | Normal d -> d.Open
                          | Extended d -> d.Open) data
    SimulateFake bot (fun f -> if f >= uint64 datf.LongLength
                               then None
                               else Some datf.[int f]) risk budget lookback
