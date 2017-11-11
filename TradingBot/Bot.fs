module TradingBot.Bot


type BotAction =
    | Wait
    | Sell of float
    | Buy of float

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
        LookBack : Timestamp
        LastAction : BotAction
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

let Initial r b t l =
    {
        Risk = r
        Budget = b
        AssetCount = 0UL
        TimestampBuy = 0UL
        TimestampSell = 0UL
        ExchangeRate = Some t
        LookBack = l
        LastAction = Wait
    }

let BuyPercentage (t : Timestamp) (rate : float) (perc : float) (prev : State) =
    let buyvolume = prev.Budget * perc / rate
                    |> floor
                    |> uint64
    {
        Risk = prev.Risk
        Budget = prev.Budget - (float buyvolume * rate)
        AssetCount = prev.AssetCount + buyvolume
        TimestampBuy = if buyvolume > 0UL then t else prev.TimestampBuy
        TimestampSell = prev.TimestampSell
        ExchangeRate = Some rate
        LookBack = prev.LookBack
        LastAction = if buyvolume > 0UL then Buy perc else Wait
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
        TimestampSell = if sellvolume > 0UL then t else prev.TimestampSell
        ExchangeRate = Some rate
        LookBack = prev.LookBack
        LastAction = if sellvolume > 0UL then Sell perc else Wait
    }
    
let TradeHistory (bot : IBot) (rate : ExchangeRate) budget risk lookback =
    let func (t, s) =
        let t = t + 1UL
        t, (bot.Next) t rate s
    let hist = iterate func (0UL, Initial risk budget (rate 0UL).Value lookback)

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
            LookBack = prev.LookBack
            ExchangeRate = Some rate
            LastAction = Wait
        }

    interface IBot with
        member __.Next t rate prev =
            let M_L o =
                if t > prev.LookBack then
                    let M_D = [t - prev.LookBack .. t]
                              |> List.map (fun t -> (rate (t - o), rate (t + 1UL - o)))
                              |> List.filter (fun (r0, r1) -> match r0, r1 with
                                                              | Some r0, Some r1 -> r1 <= r0
                                                              | _ -> false)
                              |> List.length
                    let M_I = int prev.LookBack - M_D
                    float M_D / float(M_D + M_I) / 2.0
                else
                    0.0
            let frate t = (rate t).Value
            let risk = prev.Risk
            let irisk = 1.0 - risk
            let is_risky = M_L 1UL > risk

            match rate t with
            | Some r0 -> let keep = update r0
                         if t < max 3UL prev.LookBack then              keep prev // KEEP
                         else match frate(t - 1UL), frate(t - 2UL) with
                              | r1, r2 when r0 = r1 && r1 >= r2 ->      BuyPercentage t r0 1.0 prev // BUY 100%
                              | r1, r2 when r0 < r1 && r0 >  r2 ->      keep prev // KEEP
                              | r1, r2 when r0 > r1 && r0 >= r2 ->      BuyPercentage t r0 1.0 prev // BUY 100%
                              | r1, r2 when r0 > r1 && r1 >= r2 ->      BuyPercentage t r0 risk prev // BUY R%
                              | r1, r2 when r0 > r1 && r1 <  r2 ->      BuyPercentage t r0 risk prev // BUY R%
                              | r1, r2 ->                               SellPercentage t r0 (if r0 > (frate prev.TimestampBuy) + abs(r0 - r1)
                                                                                             then irisk
                                                                                             else 1.0) prev // SELL
            | None -> match prev.ExchangeRate with
                      | Some r -> if prev.AssetCount > 0UL then SellPercentage t r 1.0 prev
                                                           else {
                                                                    Risk = risk
                                                                    Budget = prev.Budget
                                                                    AssetCount = 0UL
                                                                    TimestampBuy = prev.TimestampBuy
                                                                    TimestampSell = prev.TimestampSell
                                                                    LookBack = prev.LookBack
                                                                    ExchangeRate = None
                                                                    LastAction = Wait
                                                                }
                      | None -> prev
