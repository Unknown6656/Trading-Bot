module TradingBot.Bot


type Timestamp = uint64

type Rate = float option

type ExchangeRate = uint64 -> Rate

type AbstractRate =
    {
        Delegate : float -> float
        Max : float
    }

type State = 
    {
        Risk : float
        Budget : float
        AssetCount : uint64
        TimestampBuy : Timestamp
        TimestampSell : Timestamp
        ExchangeRate : Rate
    }

type IBot =
    // abstract member BuyPercentage : Timestamp -> float -> float -> State -> State
    // abstract member SellPercentage : Timestamp -> float -> float -> State -> State
    abstract member Next : Timestamp -> ExchangeRate -> State -> State


let create (a : AbstractRate) : obj =
    fun (x : uint64) ->
        let fx = float x
        if fx > a.Max then None
        else Some <| a.Delegate fx
    |> box
    
let splitwhen f l =
    let inv = fun x -> not <| f x
    Seq.takeWhile inv l, Seq.skipWhile inv l

let rec iterate f value =
    seq { 
            yield value
            yield! iterate f (f value)
        }

let Initial r b t =
    {
        Risk = r
        Budget = b
        AssetCount = 0UL
        TimestampBuy = 0UL
        TimestampSell = 0UL
        ExchangeRate = Some t
    }

let BuyPercentage (t : Timestamp) (rate : float) (perc : float) (prev : State) =
    let buyvolume = prev.Budget * perc / rate
                    |> floor
                    |> uint64
    {
        Risk = prev.Risk
        Budget = prev.Budget - (float buyvolume * rate)
        AssetCount = prev.AssetCount + buyvolume
        TimestampBuy = t
        TimestampSell = prev.TimestampSell
        ExchangeRate = Some rate
    }

let SellPercentage (t : Timestamp) (rate : float) (perc : float) (prev : State) =
    let perc = if perc < 0.0 then 0.0
               elif perc > 1.0 then 1.0
               else perc
    let sellvolume = float prev.AssetCount * perc
                     |> floor
                     |> uint64
    {
        Risk = prev.Risk
        Budget = prev.Budget + (float sellvolume * rate)
        AssetCount = prev.AssetCount - sellvolume
        TimestampBuy = prev.TimestampBuy
        TimestampSell = t
        ExchangeRate = Some rate
    }
    
let TradeHistory (bot : IBot) (rate : ExchangeRate) budget risk =
    let func (t, s) =
        let t = t + 1UL
        t, (bot.Next) t rate s
    let hist = iterate func (0UL, Initial risk budget (rate 0UL).Value)

    splitwhen (fun (_, s) -> s.ExchangeRate.IsNone) hist
    |> fst
    |> Seq.toList


type Bot_01 () =
    let update (rate : float) (prev : State) =
        {
            Risk = prev.Risk
            Budget = prev.Budget
            AssetCount = prev.AssetCount
            TimestampBuy = prev.TimestampBuy
            TimestampSell = prev.TimestampSell
            ExchangeRate = Some rate
        }

    interface IBot with
        member __.Next t rate prev =
            match rate t with
            | Some r0 -> let irisk = 1.0 - prev.Risk
                         if t < 2UL then update r0 prev
                         else match rate(t - 1UL), rate(t - 2UL) with
                              | Some r1, _ when r0 = r1 -> update r0 prev
                              | Some r1, Some r2 when r0 > r1 && r1 > r2 -> BuyPercentage t r0 1.0 prev
                              | Some r1, Some r2 when r0 > r1 && r1 <= r2 -> BuyPercentage t r0 irisk prev
                              | _ -> SellPercentage t r0 (if Some r0 > rate prev.TimestampBuy then irisk else 1.0) prev
            | None -> match prev.ExchangeRate with
                      | Some r -> if prev.AssetCount > 0UL then SellPercentage t r 1.0 prev
                                                           else {
                                                                    Risk = prev.Risk
                                                                    Budget = prev.Budget
                                                                    AssetCount = 0UL
                                                                    TimestampBuy = prev.TimestampBuy
                                                                    TimestampSell = prev.TimestampSell
                                                                    ExchangeRate = None
                                                                }
                      | None -> prev
