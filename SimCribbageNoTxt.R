###This version simulates a cribbage game when all discards are completely random.
#The final score is determined by the moment when a player's points are locked in.


#This particular sim eliminates all printed text to support several iterations of the code
#Each run currently returns the scores
#To see the actual individual hand results, it may be better to run SimCribbage.R

## Setup of the Deck of Cards, including the numerical value, suit, and card name
SimCribNT <- function() {
  #Score Start
  Player1Score <- 0
  Player2Score <- 0
  winner <- 0
  WhoseCrib <- sample(2,1)
  while (winner==0) {
    #Dealing Cards at the beginning of the round
    Crib <- as.data.frame(NULL)
    Delt <- sample(52,13,replace=FALSE)
    Player1Hand <- DeckOfCards[Delt[1:6],]
    Player2Hand <- DeckOfCards[Delt[7:12],]
    Starter <- DeckOfCards[Delt[13],]
    rm(Delt)
    Crib[1:2,1:4] <- Player1Hand[1:2,]
    Crib[3:4,1:4] <- Player2Hand[1:2,]
    Player1Hand <- Player1Hand[3:6,]
    Player2Hand <- Player2Hand[3:6,]
    if (Starter$COrder==11) {
      if (WhoseCrib==1) {
        Player1Score = Player1Score + 2
        winner <- CheckIfWin(Player1Score,1,winner)
      }
      else {
        Player2Score = Player2Score + 2
        winner <- CheckIfWin(Player2Score,2,winner)
      }
      if (winner>0) {break} #As soon as there is a winner, break out of the overall while loop
    }
    ##The Count
    BothCantPlay <- c(FALSE,FALSE)
    runningCount <- 0
    tempStorage <- as.data.frame(NULL)
    PeggingHand1 <- Player1Hand
    PeggingHand2 <- Player2Hand
    if (WhoseCrib==1) {
      Player1Playing <- FALSE
    } else {
      Player1Playing <- TRUE
    }
    while ((length(PeggingHand1$CValue)+length(PeggingHand2$CValue))>0) {
      if (runningCount==31) {
        runningCount = 0 #reset running count if there's a 31
        tempStorage = as.data.frame(NULL)
        BothCantPlay <- c(FALSE, FALSE)
      }
      if ((sum((PeggingHand1$CValue + runningCount)<32)==0)||(PeggingHand1$CValue==0)) {
        BothCantPlay[1] <- TRUE #Player 1 can't place a card
      }
      if ((sum((PeggingHand2$CValue + runningCount)<32)==0)||(PeggingHand2$CValue==0)) {
        BothCantPlay[2] <- TRUE #Player 2 can't place a card
      }
      if (BothCantPlay[1] && BothCantPlay[2]) { #If both cant put down a card, add 1 for go)
        if (Player1Playing) { #if player 1 is up, then player 2 placed last card, +1
          Player2Score = Player2Score + 1
          winner <- CheckIfWin(Player2Score,2,winner)
        } else {
          Player1Score = Player1Score + 1
          winner <- CheckIfWin(Player1Score,1,winner)
        } #otherwise player 1 gets +1
        if (winner>0) {break}
        runningCount = 0
        BothCantPlay <- c(FALSE, FALSE)
        tempStorage <- as.data.frame(NULL)
      } else if (Player1Playing) { #if one of them can go, then which one?
        if (BothCantPlay[1]) {#player 1's going, but player 1 can't place, switch
          Player1Playing <- !Player1Playing
        } else {
          PeggingResults <- Pegging(Player1Score,PeggingHand1,tempStorage,runningCount,FALSE)
          Player1Score <- PeggingResults[[1]]
          winner <- CheckIfWin(Player1Score,1,winner)
          PeggingHand1 <- PeggingResults[[2]]
          tempStorage <- PeggingResults[[3]]
          runningCount <- PeggingResults[[4]]
          Player1Playing <- !Player1Playing
        }
      } else if (BothCantPlay[2]) { #player 2's going, but player 2 can't place, switch
        Player1Playing <- !Player1Playing
      } else { 
        PeggingResults <- Pegging(Player2Score,PeggingHand2,tempStorage,runningCount,FALSE)
        Player2Score <- PeggingResults[[1]]
        winner <- CheckIfWin(Player2Score,2,winner)
        PeggingHand2 <- PeggingResults[[2]]
        tempStorage <- PeggingResults[[3]]
        runningCount <- PeggingResults[[4]]
        Player1Playing <- !Player1Playing
      }
      if (winner>0) {break}
      if ((length(PeggingHand1$CValue)+length(PeggingHand2$CValue))==0) { #loop won't repeat to add +1, so do it now
        if (Player1Playing) { #if player 1 is up, then player 2 placed last card, +1
          Player2Score = Player2Score + 1
          winner <- CheckIfWin(Player2Score,2,winner)
        } else {
          Player1Score = Player1Score + 1
          winner <- CheckIfWin(Player1Score,1,winner)
        } #otherwise player 1 gets +1
      }
    }
    if (winner>0) {break}
    ##The Show
    if (WhoseCrib==1) { #Player 1 has the crib
      WhoseCrib=2
      Combo <- TheShow(Player2Hand,Starter,FALSE)
      Player2Score <- Player2Score + Combo[[1]]
      winner <- CheckIfWin(Player2Score,2,winner)
      if (winner>0) {break}
      Combo <- TheShow(Player1Hand,Starter,FALSE)
      Player1Score <- Player1Score + Combo[[1]]
      winner <- CheckIfWin(Player1Score,1,winner)
      if (winner>0) {break}
      Combo <- TheShow(Crib,Starter,TRUE)
      Player1Score <- Player1Score + Combo[[1]]
      winner <- CheckIfWin(Player1Score,1,winner)
      if (winner>0) {break}
    } else { #Player 2 has the crib
      WhoseCrib=1
      Combo <- TheShow(Player1Hand,Starter,FALSE)
      Player1Score <- Player1Score + Combo[[1]]
      winner <- CheckIfWin(Player1Score,1,winner)
      if (winner>0) {break}
      Combo <- TheShow(Player2Hand,Starter,FALSE)
      Player2Score <- Player2Score + Combo[[1]]
      winner <- CheckIfWin(Player1Score,1,winner)
      if (winner>0) {break}
      Combo <- TheShow(Crib,Starter,TRUE)
      Player2Score <- Player2Score + Combo[[1]]
      winner <- CheckIfWin(Player2Score,2,winner)
      if (winner>0) {break}
    }
    if (winner>0) {break}
  }
  if (Player1Score>120) {Player1Score <- 121}
  if (Player2Score>120) {Player2Score <- 121}
  SimResults <- list(Player1Score, Player2Score) #Any data of interest that should be analyzed goes here
  return(SimResults)
}
