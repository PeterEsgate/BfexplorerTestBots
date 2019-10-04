module BfexplorerBot

(*
#I @"C:\Program Files (x86)\BeloSoft\Bfexplorer\"

#r "BeloSoft.Data.dll"
#r "BeloSoft.Betfair.API.dll"
#r "BeloSoft.Bfexplorer.Domain.dll"
#r "BeloSoft.Bfexplorer.Trading.dll"
#r "BeloSoft.Bfexplorer.Service.Core.dll"
*)

open System

open BeloSoft.Data
open BeloSoft.Bfexplorer.Domain
open BeloSoft.Bfexplorer.Trading

let toIndexes (value : string) =
    try
        value.Split(',') |> Array.map Int32.Parse |> List.ofArray
    with
    | _ -> List.empty

/// <summary>
/// MySelection
/// </summary>
type MySelection(selection : Selection) =
    
    let mutable startPrice = selection.LastPriceTraded
    let mutable startPosition = 0y
    let mutable oddsDifference = 0
    let mutable positionDifference = 0y

    member _this.Id
        with get() = selection.Id

    member _this.Selection
        with get() = selection

    member _this.StartPosition
        with get() = startPosition

    member _this.OddsDifference
        with get() = oddsDifference

    member _this.PositionDifference
        with get() = positionDifference

    member _this.SetStartPrice() =
        startPrice <- selection.LastPriceTraded

    member _this.SetStartPosition(position : sbyte) =
        startPosition <- position

    member _this.SetPositionDifference(position : sbyte) =
        positionDifference <- startPosition - position

    member _this.SetOddsDifference() =
        oddsDifference <- selection.OddsContext.GetOddsDifference(startPrice, selection.LastPriceTraded)
    
/// <summary>
/// TriggerStatus
/// </summary>
type TriggerStatus =
    | Initialize
    | WaitForRaceStart
    | WaitForStartTradingSession

/// <summary>
/// HorseRacingBotTrigger
/// </summary>
type HorseRacingBotTrigger(market : Market, _selection : Selection, _botName : string, botTriggerParameters : BotTriggerParameters, myBfexplorer : IMyBfexplorer) =

    let mutable allowedSelectionIndexes = toIndexes (defaultArg (botTriggerParameters.GetParameter<string>("AllowedSelectionIndexes")) "0")
    let timeToStart = defaultArg (botTriggerParameters.GetParameter<float>("TimeToStart")) 40.0
    let favouritesPrice = defaultArg (botTriggerParameters.GetParameter<float>("FavouritesPrice")) 50.0

    let mutable stake = defaultArg (botTriggerParameters.GetParameter<float>("Stake")) 5.0
    let mutable betType = defaultArg (botTriggerParameters.GetParameter<string>("BetType")) "Back"
    let mutable strategy = defaultArg (botTriggerParameters.GetParameter<int>("Strategy")) 0
    let mutable status = TriggerStatus.Initialize
    let mutable mySelections = nil<MySelection list>
    let mutable startTime = DateTime.MinValue

    let outputMessage message =
        myBfexplorer.BfexplorerService.OutputMessage(message, market.Id)

    let isHorseRacingMarket() =
        market.MarketInfo.BetEventType.Id = 7 && market.MarketDescription.MarketType = "WIN"

    let getActiveSelections() =
        market.Selections |> Seq.filter isActiveSelection |> Seq.toList

    let getFavourites() = 
        getActiveSelections() 
        |> List.sortBy (fun mySelection -> mySelection.LastPriceTraded)

    let getXFavourites count = 
        let favourites = getFavourites()
        
        favourites |> List.take (min count favourites.Length)

    let getMySelections() = maybe {
       
            let selections = getActiveSelections()
            let mumberOfSelections = selections.Length
            let maximalIndex = allowedSelectionIndexes |> List.max
       
            if maximalIndex < 1 then
                 match mumberOfSelections with
                 | 6|5|4|3|2|1 -> allowedSelectionIndexes <- toIndexes ("1,2,3,4,5,6")  
                                  strategy <- 1
                                  return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | 7|8 -> allowedSelectionIndexes <- toIndexes ("3,4,5")
                          strategy <- 1
                          return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | 9 -> allowedSelectionIndexes <- toIndexes ("1,2,8,9")  
                        strategy <- 2
                        return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | 10 -> allowedSelectionIndexes <- toIndexes ("1,2,9,10")  
                         strategy <- 2
                         return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | 11 -> allowedSelectionIndexes <- toIndexes ("1,2,3,9,10,11")  
                         strategy <- 2
                         return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | 12 -> allowedSelectionIndexes <- toIndexes ("1,2,3,10,11,12")  
                         strategy <- 2
                         return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])  
                 | _ -> allowedSelectionIndexes <- toIndexes ("1,2,3,4")   
                        strategy <- 2
                        return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])
            else
            strategy <- 1
            return allowedSelectionIndexes |> List.map (fun index -> selections.[index - 1])
    }

    let setSelectionPositions (setPosition : sbyte -> MySelection -> unit) (forMySelections : MySelection list) =
        getFavourites()
        |> List.iteri (fun index aSelection ->
                let selectionId = aSelection.Id

                forMySelections 
                |> List.tryFind (fun mySelection -> mySelection.Id = selectionId)
                |> Option.iter (setPosition (sbyte (index + 1)))
            )

    let reportMySelections (toString : MySelection -> string) (forMySelections : MySelection list) =
        forMySelections |> List.map toString |> String.concat "\n" |> outputMessage        

    let initialize() =
        if isHorseRacingMarket()
        then
            match getMySelections() with
            | Some selections ->

                status <- TriggerStatus.WaitForRaceStart
                mySelections <- selections |> List.map MySelection

                TriggerResult.WaitingForOperation

            | None -> TriggerResult.EndExecutionWithMessage "Failed to make my selections!"
        else
            TriggerResult.EndExecutionWithMessage "You can run this bot on a horse racing market only!"

    let isTimeToStart() =
        (DateTime.Now - startTime).TotalSeconds >= timeToStart

    let setStartData() =
        mySelections |> setSelectionPositions (fun position mySelection -> mySelection.SetStartPrice(); mySelection.SetStartPosition(position))
        
        mySelections
        |> List.sortBy (fun mySelection -> mySelection.StartPosition)
        |> reportMySelections (fun mySelection -> sprintf "%d. %s" mySelection.StartPosition mySelection.Selection.Name)

        status <- TriggerStatus.WaitForStartTradingSession
        startTime <- DateTime.Now        

    let getMySelection() = maybe {
        let favourites = getXFavourites 3
        if strategy = 2 then
///Test that odds of first and second favourite < = favouritesPrice
          if favourites.Length >= 2 && (favourites |> List.take 2 |> List.forall (fun mySelection -> mySelection.LastPriceTraded <= favouritesPrice))
          then
            mySelections |> setSelectionPositions (fun position mySelection -> mySelection.SetOddsDifference(); mySelection.SetPositionDifference(position))
///I Cant see where it removes the selection if the selection has improved in position
            let mySortedSelections = mySelections |> List.sortBy (fun mySelection -> mySelection.PositionDifference)

            mySortedSelections |> reportMySelections (fun mySelection -> sprintf "%s: %d" mySelection.Selection.Name mySelection.PositionDifference)

            let mySelection = (mySortedSelections |> List.head).Selection
            let mySelectionId = mySelection.Id
///test that our selection NOT in top 3
            if not (favourites |> List.exists (fun favourite -> favourite.Id = mySelectionId))
            then
                return mySelection
///Else Must Be strategy 1
        else
///Test that odds of first and second favourite > = favouritesPrice
            if favourites.Length >= 2 && (favourites |> List.take 2 |> List.forall (fun mySelection -> mySelection.LastPriceTraded >= favouritesPrice))
            then
              mySelections |> setSelectionPositions (fun position mySelection -> mySelection.SetOddsDifference(); mySelection.SetPositionDifference(position))

              let mySortedSelections = mySelections |> List.sortBy (fun mySelection -> mySelection.PositionDifference)

              mySortedSelections |> reportMySelections (fun mySelection -> sprintf "%s: %d" mySelection.Selection.Name mySelection.PositionDifference)

              let mySelection = (mySortedSelections |> List.head).Selection
              let mySelectionId = mySelection.Id
///test that our selection IS in top 3
              if (favourites |> List.exists (fun favourite -> favourite.Id = mySelectionId))
              then
                return mySelection
    }

    interface IBotTrigger with

        member __.Execute() =
            match status with
            | TriggerStatus.Initialize -> initialize()
            | TriggerStatus.WaitForRaceStart -> 
            
                if market.IsInPlay
                then
                    setStartData()

                TriggerResult.WaitingForOperation

            | TriggerStatus.WaitForStartTradingSession -> 

                if isTimeToStart() && strategy = 2
                then
                    match getMySelection() with
                    | Some mySelection -> 
                          stake <- 2.0
                          betType <- "Lay"
                          let myBotParameters = [ { MyBotParameter.Name = "Stake"; MyBotParameter.Value = stake };{ MyBotParameter.Name = "BetType"; MyBotParameter.Value = betType  } ]
                          TriggerResult.ExecuteActionBotOnSelectionWithParameters (mySelection, myBotParameters) 
                    | None -> TriggerResult.EndExecution
              
                elif isTimeToStart() && strategy = 1
                then
                    match getMySelection() with
                    | Some mySelection -> 
                          stake <- 5.0
                          betType <- "Place"
                          let myBotParameters = [ { MyBotParameter.Name = "Stake"; MyBotParameter.Value = stake };{ MyBotParameter.Name = "BetType"; MyBotParameter.Value = betType  } ]
                          TriggerResult.ExecuteActionBotOnSelectionWithParameters (mySelection, myBotParameters) 
                    | None -> TriggerResult.EndExecution
                else
                    TriggerResult.WaitingForOperation

        member __.EndExecution() =
            ()