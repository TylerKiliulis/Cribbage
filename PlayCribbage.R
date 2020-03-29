#This version has no strategy when it comes to determining which cards should be chosen during the play and the show
#These processes are random in order to see if I can simulate a whole game

## Setup of the Deck of Cards, including the numerical value, suit, and card names
PlayCrib <- function() {
#Score Start
Player1Score <- 0
Player2Score <- 0
winner <- 0
WhoseCrib <- sample(2,1)
while (winner==0) {
  #Dealing Cards at the beginning of the round
  print(noquote(paste("Player" , WhoseCrib, "has the crib", sep= " ")))
  Crib <- as.data.frame(NULL)
  Delt <- sample(52,13,replace=FALSE)
  Player1Hand <- DeckOfCards[Delt[1:6],]
  Player2Hand <- DeckOfCards[Delt[7:12],]
  Starter <- DeckOfCards[Delt[13],]
  rm(Delt)
  AfterTheToss <- TossCards(Player1Hand) #prompts players
  Player1Hand <- AfterTheToss[[2]]
  Crib <- AfterTheToss[[1]]
  Crib[3:4,1:4] <- Player2Hand[1:2,] #Player 2 isn't actually playing, random cards in crib
  Player2Hand <- Player2Hand[3:6,]
  print.noquote("========================================================")
  print(dispcard <- DisplayCard(Starter,1))
  if (Starter$COrder==11) {
    print("Jack for 2")
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
  print.noquote("========================================================")
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
        print.noquote("Go")
        Player2Score = Player2Score + 1
        print.noquote(paste("Current Score:",Player1Score,"-",Player2Score,sep = " "))
        winner <- CheckIfWin(Player2Score,2,winner)
      } else {
        print.noquote("Go")
        Player1Score = Player1Score + 1
        print.noquote(paste("Current Score:",Player1Score,"-",Player2Score,sep = " "))
        winner <- CheckIfWin(Player1Score,1,winner)
      } #otherwise player 1 gets +1
      if (winner>0) {break}
      runningCount = 0
      BothCantPlay <- c(FALSE, FALSE)
      tempStorage <- as.data.frame(NULL)
    } else if (Player1Playing) { #if one of them can go, then which one?
      if (BothCantPlay[1]) {#player 1's going, but player 1 can't place, switch
        print.noquote("Go")
        Player1Playing <- !Player1Playing
      } else {
        PeggingResults <- Pegging(Player1Score,PeggingHand1,tempStorage,runningCount,TRUE)
        Player1Score <- PeggingResults[[1]]
        winner <- CheckIfWin(Player1Score,1,winner)
        PeggingHand1 <- PeggingResults[[2]]
        tempStorage <- PeggingResults[[3]]
        print.noquote(" ")
        print(dispcard <- DisplayCard(tempStorage,2))
        print.noquote(" ")
        runningCount <- PeggingResults[[4]]
        print.noquote(paste(runningCount,PeggingResults[[5]]))
        Player1Playing <- !Player1Playing
      }
    } else if (BothCantPlay[2]) { #player 2's going, but player 2 can't place, switch
      print.noquote("Go")
      Player1Playing <- !Player1Playing
    } else { 
      PeggingResults <- Pegging(Player2Score,PeggingHand2,tempStorage,runningCount,FALSE)
      Player2Score <- PeggingResults[[1]]
      winner <- CheckIfWin(Player2Score,2,winner)
      PeggingHand2 <- PeggingResults[[2]]
      tempStorage <- PeggingResults[[3]]
      print.noquote(" ")
      print(dispcard <- DisplayCard(tempStorage,2))
      print.noquote(" ")
      runningCount <- PeggingResults[[4]]
      print.noquote(paste(runningCount,PeggingResults[[5]]))
      Player1Playing <- !Player1Playing
    }
    if (winner>0) {break}
    if ((length(PeggingHand1$CValue)+length(PeggingHand2$CValue))==0) { #loop won't repeat to add +1, so do it now
      print.noquote("Last Card for 1")
      if (Player1Playing) { #if player 1 is up, then player 2 placed last card, +1
        Player2Score = Player2Score + 1
        print.noquote(paste("Current Score:",Player1Score,"-",Player2Score,sep = " "))
        winner <- CheckIfWin(Player2Score,2,winner)
      } else {
        Player1Score = Player1Score + 1
        print.noquote(paste("Current Score:",Player1Score,"-",Player2Score,sep = " "))
        winner <- CheckIfWin(Player1Score,1,winner)
      } #otherwise player 1 gets +1
    }
  }
  if (winner>0) {break}
  ##The Show
  print.noquote("========================================================")
  print(dispcard <- DisplayCard(Starter,1))
  print.noquote(" ")
  if (WhoseCrib==1) { #Player 1 has the crib
    WhoseCrib=2
    Combo <- TheShow(Player2Hand,Starter,FALSE)
    print(dispcard <- DisplayCard(Player2Hand,4))
    print.noquote(Combo[[2]])
    print.noquote(" ")
    Player2Score <- Player2Score + Combo[[1]]
    winner <- CheckIfWin(Player2Score,2,winner)
    if (winner>0) {break}
    readline(prompt = "Press enter to continue ")
    Combo <- TheShow(Player1Hand,Starter,FALSE)
    print(dispcard <- DisplayCard(Player1Hand,3))
    print.noquote(Combo[[2]])
    print.noquote(" ")
    Player1Score <- Player1Score + Combo[[1]]
    winner <- CheckIfWin(Player1Score,1,winner)
    if (winner>0) {break}
    readline(prompt = "Press enter to continue ")
    Combo <- TheShow(Crib,Starter,TRUE)
    print(dispcard <- DisplayCard(Crib,5))
    print.noquote(Combo[[2]])
    Player1Score <- Player1Score + Combo[[1]]
    winner <- CheckIfWin(Player1Score,1,winner)
    if (winner>0) {break}
  } else { #Player 2 has the crib
    WhoseCrib=1
    Combo <- TheShow(Player1Hand,Starter,FALSE)
    print(dispcard <- DisplayCard(Player1Hand,3))
    print.noquote(Combo[[2]])
    print.noquote(" ")
    Player1Score <- Player1Score + Combo[[1]]
    winner <- CheckIfWin(Player1Score,1,winner)
    if (winner>0) {break}
    readline(prompt = "Press enter to continue ")
    Combo <- TheShow(Player2Hand,Starter,FALSE)
    print(dispcard <- DisplayCard(Player2Hand,4))
    print.noquote(Combo[[2]])
    print.noquote(" ")
    Player2Score <- Player2Score + Combo[[1]]
    winner <- CheckIfWin(Player2Score,2,winner)
    if (winner>0) {break}
    readline(prompt = "Press enter to continue ")
    Combo <- TheShow(Crib,Starter,TRUE)
    print(dispcard <- DisplayCard(Crib,6))
    print.noquote(Combo[[2]])
    Player2Score <- Player2Score + Combo[[1]]
    winner <- CheckIfWin(Player2Score,2,winner)
    if (winner>0) {break}
  }
  if (winner>0) {break}
  print.noquote(paste("Current Score:",Player1Score,"-",Player2Score,sep=" "))
  readline(prompt = "Press enter to continue ")
  print.noquote("========================================================")
}
print.noquote("========================================================")
if (Player1Score>120) {Player1Score <- 121}
if (Player2Score>120) {Player2Score <- 121}
print.noquote(paste("Player",winner,"has reached 121 points first and has won the game!",sep = " "))
print.noquote(paste("The final Score is",Player1Score,"-",Player2Score,sep = " "))
}
