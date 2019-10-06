module BfexplorerBot



//(*
#I @"C:\Program Files (x86)\BeloSoft\Bfexplorer\"

#r "BeloSoft.Data.dll"
#r "BeloSoft.Betfair.API.dll"
#r "BeloSoft.Bfexplorer.Domain.dll"
#r "BeloSoft.Bfexplorer.Trading.dll"
#r "BeloSoft.Bfexplorer.Service.Core.dll"
//*)

open System
open BeloSoft.Data
open BeloSoft.Bfexplorer.Domain
open BeloSoft.Bfexplorer.Trading
open System.Collections.Generic

/// <summary>
/// TriggerStatus
/// </summary>
type TriggerStatus =
    | Initialize
    | WaitForPreviousResult
    | WaitForRaceStart
    | WaitForStartTradingSession
    | WaitForStartTradingSessionPick1
    | WaitForStartTradingSessionPick2
    | WaitForStartTradingSessionPick3
    | WaitForStartTradingSessionPick4
 





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

    member _this.LPT
       with get() = startPrice
   
/// <summary>
/// HorseRacingBotTrigger
/// </summary>
type BotTrigger(market : Market, _selection : Selection, _botName : string, botTriggerParameters : BotTriggerParameters, myBfexplorer : IMyBfexplorer) =

    let myStrategyResults = myBfexplorer.BfexplorerService |> MyStrategyOperations.RegisterMyStrategy "BotTrigger"
   // let myStrategyResults = MyStrategyOperations.GetMyStrategyResults("BotTrigger")
    let timeToStart = defaultArg (botTriggerParameters.GetParameter<float>("TimeToStart")) 40.0
    let favouritesPrice = defaultArg (botTriggerParameters.GetParameter<float>("FavouritesPrice")) 50.0

    let mutable status = TriggerStatus.Initialize
    let mutable startTime = DateTime.MinValue
    let mutable mySelections = nil<MySelection list>
    let mutable mySortedSelections = nil<MySelection list>


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

    let getWorsts() = 
        getActiveSelections() 
        |> List.sortBy (fun mySelection -> mySelection.LastPriceTraded)
        |> List.rev

    let getXWorsts count = 
        let worsts = getWorsts()
        worsts |> List.take (min count worsts.Length)

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
            status <- TriggerStatus.WaitForPreviousResult
           
            mySelections <- getActiveSelections() |> List.map MySelection
           
            TriggerResult.WaitingForOperation
        else
            TriggerResult.EndExecutionWithMessage "You can run this bot on a horse racing market only!"

    let waitForPreviousResult() =
        let waitingForOperation =
            if market.IsInPlay
            then
                false
            else
                if myStrategyResults.CanExecuteMyStrategy
                then
                    status <- TriggerStatus.WaitForRaceStart

                    match myStrategyResults.BettingStreak with
                    | BettingStreak.LosingStreak numberOfLosses -> numberOfLosses <= 3
                    | _ -> true
                else
                    true

        if waitingForOperation
        then
            TriggerResult.WaitingForOperation
        else
            TriggerResult.EndExecution

    let setStartData() =
        if market.IsInPlay 
        then
            status <- TriggerStatus.WaitForStartTradingSession
            startTime <- DateTime.Now 
            mySelections |> setSelectionPositions (fun position mySelection -> mySelection.SetStartPrice(); mySelection.SetStartPosition(position))

            mySortedSelections <- mySelections   |> List.sortBy (fun mySelection -> mySelection.StartPosition)
        
            mySortedSelections    |> reportMySelections (fun mySelection -> sprintf "%d. %s" mySelection.StartPosition mySelection.Selection.Name)
        TriggerResult.WaitingForOperation
        


    let isTimeToStart() =
        (DateTime.Now - startTime).TotalSeconds >= timeToStart

/////////still need to rearrange order based on conditions
    let sortMySelections() =
          let selections = getActiveSelections()
          let numberOfSelections = selections.Length

          let favourites = getXFavourites numberOfSelections

          if isTimeToStart() && favourites.Length >= 2 && (favourites |> List.take 2 |> List.forall (fun mySelection -> mySelection.LastPriceTraded <= favouritesPrice))
          then
              status <- TriggerStatus.WaitForStartTradingSessionPick1
              mySelections |> setSelectionPositions (fun position mySelection -> mySelection.SetOddsDifference(); mySelection.SetPositionDifference(position))

              mySortedSelections <- mySelections |> List.sortBy (fun mySelection -> mySelection.OddsDifference) 
           
                      
              mySortedSelections  |> reportMySelections (fun mySelection -> sprintf "%s: %d %f" mySelection.Selection.Name mySelection.PositionDifference mySelection.LPT)
          TriggerResult.WaitingForOperation



    interface IBotTrigger with

        member __.Execute() =
            match status with
            | TriggerStatus.Initialize -> initialize()
            | TriggerStatus.WaitForPreviousResult -> waitForPreviousResult()
            | TriggerStatus.WaitForRaceStart -> setStartData()
                 
                
            | TriggerStatus.WaitForStartTradingSession -> sortMySelections()
            | TriggerStatus.WaitForStartTradingSessionPick1 -> 
                   status <- TriggerStatus.WaitForStartTradingSessionPick2 
                   let mySelection = (mySortedSelections |> List.item 3).Selection
                   
               //    myStrategyResults.AddMarket(market)
                   let myBotParameters = [ { MyBotParameter.Name = "Stake"; MyBotParameter.Value = 2.0 } ;{ MyBotParameter.Name = "BetType"; MyBotParameter.Value = "Lay"}]                   
                   TriggerResult.ExecuteActionBotOnSelectionWithParameters (mySelection, myBotParameters)
     
            | TriggerStatus.WaitForStartTradingSessionPick2 -> 
                   status <- TriggerStatus.WaitForStartTradingSessionPick3
                   let mySelection1 = (mySortedSelections |> List.item 4).Selection
                             
              //     myStrategyResults.AddMarket(market)
                                             
                   let myBotParameters = [ { MyBotParameter.Name = "Stake"; MyBotParameter.Value = 3.0 } ;{ MyBotParameter.Name = "BetType"; MyBotParameter.Value = "Back"}]                   
                   TriggerResult.ExecuteActionBotOnSelectionWithParameters (mySelection1, myBotParameters)
 
            | TriggerStatus.WaitForStartTradingSessionPick3 -> 
                   status <- TriggerStatus.WaitForStartTradingSessionPick4
                   let mySelection2 = (mySortedSelections |> List.item 5).Selection
               
               //    myStrategyResults.AddMarket(market)
                               
                   TriggerResult.ExecuteActionBotOnSelection mySelection2

            | TriggerStatus.WaitForStartTradingSessionPick4 -> 
               
                   let mySelection3 = (mySortedSelections |> List.item 1).Selection
            
               //    myStrategyResults.AddMarket(market)
                            
                   TriggerResult.ExecuteActionBotOnSelection mySelection3

        

     

        member __.EndExecution() =
            ()
