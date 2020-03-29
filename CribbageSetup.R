CribSetup <- function() { 
CValue <- COrder <- rep(1:13,4)
CValue[CValue > 10]=10
CName <- rep(c("Ace","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King"),4)
CSuit <- c(rep("Hearts",13),rep("Diamonds",13),rep("Spades",13),rep("Clubs",13))
CFullName <- paste(CName,CSuit, sep=" of ")
DeckOfCards <- data.frame(CFullName,CValue,CSuit,COrder)
rm(CFullName,CName,COrder,CSuit,CValue)
return(DeckOfCards)
}
DeckOfCards <- CribSetup()
Crib <- data.frame(NULL)
FifteenScore <- function(Fifteens) { #function that adds up player's 15's
  Score15 = 0
  bigvectah <- NULL
  AddUp2 = combn(5,2) #all possible combinations of 15's
  AddUp3 = combn(5,3)
  AddUp4 = combn(5,4)
  AddUp5 = combn(5,5)
  for (i in 1:10) {
   bigvectah[i] = sum(Fifteens[AddUp2[,i]])
  }
  for (i in 11:20) {
    bigvectah[i] = sum(Fifteens[AddUp3[,(i-10)]])
  }
  for (i in 21:25) {
    bigvectah[i] = sum(Fifteens[AddUp4[,(i-20)]])
  }
  bigvectah[26]=sum(Fifteens)
  Score15 = 2*(sum(bigvectah==15))
  if (Score15==0) {
    Callout15 <- ""
  } else {Callout15 <- paste((Score15/2),"times 15 for", Score15, sep = " ")}
  Scoring1 <- list(Score15,Callout15)
  return(Scoring1)
}
PairScore <- function(Pairs,tempScore) { #Function that adds up a player's pairs
  ScorePair = 0
  specialPair = 0
  if (length(unique(Pairs))==3) { #Either 3 of a kind or 2 different pairs (could shorten)
    if (sum(unique(Pairs)[1]==Pairs)>2) { #Case #1 for 3 of a kind
     ScorePair = 6
     specialPair = 1
    } else if (sum(unique(Pairs)[2]==Pairs)>2) { #Case #2 for 3 of a kind
     ScorePair = 6
     specialPair = 1
    } else if (sum(unique(Pairs)[3]==Pairs)>2) { #Case #3 for 3 of a kind
      ScorePair = 6
      specialPair = 1
    } else (ScorePair = 4) #Otherwise, 2 different pairs
  } else if (length(unique(Pairs))==4) { #Must be 1 pair in here somewhere
    ScorePair = 2
  }
  if (ScorePair==0) {
    CalloutPair <- as.character(NULL)
  } else if (ScorePair==2) {
    CalloutPair <- paste(1, "Pair for", 2, sep = " ")
  } else {CalloutPair <- paste((ScorePair/2),"Pairs for", (ScorePair+tempScore),sep=" ")}
  Scoring2 <- list(ScorePair,CalloutPair,specialPair)
  return(Scoring2)
}
RowScore <- function(InARow,specialPair,tempScore) { #function that adds up any 3,4, or 5 card runs
  ScoreRow=0
  CalloutRow <- as.character(NULL)
  InARow <- sort(InARow)
  NewInARow <- unique(InARow)
  if (specialPair==1) { #3 of a kind
    if (identical(NewInARow[1:3],NewInARow[1]:NewInARow[3])) {
      ScoreRow = 9
      CalloutRow <- paste(3, "three card runs for", ScoreRow+tempScore,sep=" ")
    }
  } else if (length(NewInARow)==3) { #2 different pairs
    if (identical(NewInARow[1:3],NewInARow[1]:NewInARow[3])) {
      ScoreRow = 12
      CalloutRow <- paste(4, "three card runs for", ScoreRow+tempScore,sep=" ")
    }
  } else if (length(NewInARow)==4) { #1 pair
    if (length(NewInARow[1]:NewInARow[4])==4) { #4 in a row w/ pair
      ScoreRow = 8
      CalloutRow <- paste(1, "four card run for", ScoreRow+tempScore,sep=" ")
    } else if ((length(NewInARow[1]:NewInARow[3])==3) || (length(NewInARow[2]:NewInARow[4])==3)) { #3 in a row exists
      if (((InARow[1]==InARow[2]) && (identical(InARow[3:5],InARow[3]:InARow[5]))) || ((InARow[4]==InARow[5]) && (identical(InARow[1:3],InARow[1]:InARow[3])))) {
        ScoreRow = 3 #Pair isn't in 3 in a row
        CalloutRow <- paste(1, "three card run for", ScoreRow+tempScore,sep=" ")
      } else (ScoreRow = 6) #Otherwise it is
      CalloutRow <- paste(2, "three card runs for", ScoreRow+tempScore,sep=" ")
    }
  } else if (identical(InARow[1:3],InARow[1]:InARow[3])) { #No pairs, 3 in a row?
    ScoreRow = 3
    CalloutRow <- paste(1, "three card run for", ScoreRow+tempScore,sep=" ")
  } else if (identical(InARow[2:4],InARow[2]:InARow[4])) {
    ScoreRow = 3
    CalloutRow <- paste(1, "three card run for", ScoreRow+tempScore,sep=" ")
  } else if (identical(InARow[3:5],InARow[3]:InARow[5])) {
    ScoreRow = 3
    CalloutRow <- paste(1, "three card run for", ScoreRow+tempScore,sep=" ")
  }
Scoring3 <- list(ScoreRow,CalloutRow)
return(Scoring3)
}
FlushScore <- function(Flushh,tempScore,isCrib) { #function that adds up a player's flushes
  ScoreFlush=0
  if (isCrib) {
    if (sum(Flushh[1:5]==Flushh[1])==5) {
      ScoreFlush = 5
    }
  } else if (sum(Flushh[1:4]==Flushh[1])==4) { #First 4 are original, last one is Starter
     if (sum(Flushh[1:5]==Flushh[1])==5) {
      ScoreFlush = 5
     } else (ScoreFlush = 4)
  }
  if (ScoreFlush==0) {
    CalloutFlush <- as.character(NULL)
  } else {CalloutFlush <- paste(ScoreFlush,"flush for", ScoreFlush+tempScore,sep=" ")}
  Scoring4 <- list(ScoreFlush,CalloutFlush)
  return(Scoring4)
}
JackScore <- function(RightJack,IsThereAJack,tempScore) { #function that detects a right jack
  ScoreJack=0
  AnyRJ <- NULL
  RJ <- RightJack[5]
  if (sum(IsThereAJack[1:4]==11)>0) { #Any Jacks?
    AnyRJ <- RightJack[IsThereAJack[1:4]==11]
    if (sum(AnyRJ==RJ)>0) {
      ScoreJack = 1
      CalloutJack <- paste("Right Jack for",ScoreJack+tempScore)
    } else {CalloutJack <- as.character(NULL)}
  } else {CalloutJack <- as.character(NULL)}
  Scoring5 <- list(ScoreJack,CalloutJack)
  return(Scoring5)
}
TheShow <- function(Hand,Starter,isCrib) { #function that combines all the tallying functions into 1 scoring system
  Scoring <- rbind(Hand,Starter)
  tempScore <- 0
  Callout=as.character(NULL)
  #15's
  Scoring1 <- FifteenScore(Scoring$CValue)
  tempScore = tempScore + Scoring1[[1]]
  if (Scoring1[[1]]>0) {Callout <- paste0(Callout,paste0(Scoring1[[2]],","),sep = " ")}
  #Pairs
  Scoring2 <- PairScore(Scoring$COrder,tempScore)
  specialPair <- Scoring2[[3]]
  tempScore = tempScore + Scoring2[[1]]
  if (Scoring2[[1]]>0) {Callout <- paste0(Callout,paste0(Scoring2[[2]],","),sep = " ")}
  #In a Rows
  Scoring3 <- RowScore(Scoring$COrder,specialPair,tempScore)
  tempScore = tempScore + Scoring3[[1]]
  if (Scoring3[[1]]>0) {Callout <- paste0(Callout,paste0(Scoring3[[2]],","),sep = " ")}
  #Flush
  Scoring4 <- FlushScore(Scoring$CSuit,tempScore,isCrib)
  tempScore = tempScore + Scoring4[[1]]
  if (Scoring4[[1]]>0) {Callout <- paste0(Callout,paste0(Scoring4[[2]],","),sep = " ")}
  #Right Jack
  Scoring5 <- JackScore(Scoring$CSuit,Scoring$COrder,tempScore)
  tempScore = tempScore + Scoring5[[1]]
  if (Scoring5[[1]]>0) {Callout <- paste0(Callout,paste0(Scoring5[[2]],","),sep = " ")}
  if (length(Callout)==0) {
    Callout = "Nothing"
    } else {Callout <- paste(Callout,"Player received", tempScore,"points", sep = " ")}
  DaShow <- list(tempScore,Callout)
  return(DaShow)
}
CheckIfWin <- function(TotScore,Player,winner,Player1Score,Player2Score) { #function that checks if a player has won after all key scoring occurances
  if (winner==0) {
    if (TotScore>120) {
      winner = Player
    }
  }
  return(winner)
}
TossCards <- function(Hand) { #function where the player tosses cards before the pegging
  Toss1 = 0
  Toss2 = 0
  while (Toss1==0) {
    print(dispcard <- DisplayCard(Hand,7))
    Toss1 = readline(prompt = "Select which card (1-6) you want to put in the crib: ")
    if (sum(Toss1==c(1:6))>0) { #Makes sure that the number typed in is actually valid
      Toss1=c(1:6)[Toss1==c(1:6)] #Sets the thing I typed in to an actual number
      Crib[1,1:4]= Hand[Toss1,1:4] #Puts that card in crib
      Hand = Hand[-Toss1,] #Removes card from hand
    } else (Toss1=0)
  }
  while (Toss2==0) {
    print(dispcard <- DisplayCard(Hand,7))
    Toss2 = readline(prompt = "Select which card (1-5) you want to put in the crib: ")
    if (sum(Toss2==c(1:6))>0) { #Makes sure that the number typed in is actually valid
      Toss2=c(1:6)[Toss2==c(1:6)] #Sets the thing I typed in to an actual number
      Crib[2,1:4]= Hand[Toss2,1:4] #Puts that card in crib
      Hand = Hand[-Toss2,] #Removes card from hand
    } else (Toss2=0)
  }
  AfterTheToss <- list(Crib,Hand)
  return(AfterTheToss)
}
Pegging <- function(PlayerXScore,PeggingHand,tempStorage,runningCount,needsPrompt) {
  #Place down their card
  TossThis <- 0
  if (needsPrompt) {
  while (TossThis==0) {
    print(dispcard <- DisplayCard(PeggingHand,7))
    TossThis = readline(prompt = "Select your card: ")
    if (sum(TossThis==c(1:(length(PeggingHand$COrder))))>0) { #Makes sure that the number typed in is actually valid
      TossThis=c(1:(length(PeggingHand$COrder)))[TossThis==c(1:(length(PeggingHand$COrder)))] #Sets the thing I typed in to an actual number
      if ((PeggingHand$CValue[TossThis]+runningCount)<32) {
      tempStorage[(length(tempStorage$CValue)+1),1:4]=PeggingHand[TossThis,1:4]#Puts that card down
      PeggingHand=PeggingHand[-TossThis,]#Removes card from hand
      } else (TossThis=0)
    } else (TossThis=0)
  }
  } else  {#if we don't want to ask the user which card to toss, select random card
    while (TossThis==0) {
      TossThis = sample(length(PeggingHand$CValue),1)
      if ((PeggingHand$CValue[TossThis]+runningCount)<32) {
        tempStorage[(length(tempStorage$CValue)+1),1:4]=PeggingHand[TossThis,1:4]#Puts that card down
        PeggingHand=PeggingHand[-TossThis,]#Removes card from hand
      } else (TossThis=0)
    }
  }
  runningCount = runningCount + tempStorage$CValue[length(tempStorage$CValue)]
  #Then check to see if anything happens
  #15? 31? Pair? In a rows?
  PointsGained <- PlayerXScore
  if (length(tempStorage$CValue)>1) { #Opportunity for 15's and pairs
    if ((runningCount == 15) || (runningCount == 31)) { #15's and 31's
      PlayerXScore = PlayerXScore + 2
    }
    if (tempStorage$COrder[length(tempStorage$COrder)]==tempStorage$COrder[(length(tempStorage$COrder)-1)]) {
      PlayerXScore = PlayerXScore + 2 #if last card and this card are the same then it's a pair
    }
  }
  if (length(tempStorage$CValue)>2) {
    if (sum(tempStorage$COrder[(length(tempStorage$COrder)-2):(length(tempStorage$COrder))]==tempStorage$COrder[length(tempStorage$COrder)])>2) {
      PlayerXScore = PlayerXScore + 4 # The other 2 points already awarded
    }
  }
  if (length(tempStorage$CValue)>3) {
    if (sum(tempStorage$COrder[(length(tempStorage$COrder)-3):(length(tempStorage$COrder))]==tempStorage$COrder[length(tempStorage$COrder)])>3) {
      PlayerXScore = PlayerXScore + 6 #The other 6 points already awarded
    }
  }
  NoRunYet <- TRUE
  if (length(tempStorage$COrder)>6 && NoRunYet) { #Check for runs, starting with the maximum of 7
    CheckForInARow <- sort(tempStorage$COrder[(length(tempStorage$COrder)-6):length(tempStorage$COrder)])
    if (identical(CheckForInARow,CheckForInARow[1]:CheckForInARow[7])) {
      PlayerXScore = PlayerXScore + 7
      NoRunYet <- FALSE
    }
  }
  if (length(tempStorage$COrder)>5 && NoRunYet) {#run of 6
    CheckForInARow <- sort(tempStorage$COrder[(length(tempStorage$COrder)-5):length(tempStorage$COrder)])
    if (identical(CheckForInARow,CheckForInARow[1]:CheckForInARow[6])) {
      PlayerXScore = PlayerXScore + 6
      NoRunYet <- FALSE
    }
  } 
  if (length(tempStorage$COrder)>4 && NoRunYet) {#run of 5
    CheckForInARow <- sort(tempStorage$COrder[(length(tempStorage$COrder)-4):length(tempStorage$COrder)])
    if (identical(CheckForInARow,CheckForInARow[1]:CheckForInARow[5])) {
      PlayerXScore = PlayerXScore + 5
      NoRunYet <- FALSE
    }
  } 
  if (length(tempStorage$COrder)>3 && NoRunYet) {#run of 4
    CheckForInARow <- sort(tempStorage$COrder[(length(tempStorage$COrder)-3):length(tempStorage$COrder)])
    if (identical(CheckForInARow,CheckForInARow[1]:CheckForInARow[4])) {
      PlayerXScore = PlayerXScore + 4
      NoRunYet <- FALSE
    }
  } 
  if (length(tempStorage$COrder)>2 && NoRunYet) {#run of 3
    CheckForInARow <- sort(tempStorage$COrder[(length(tempStorage$COrder)-2):length(tempStorage$COrder)])
    if (identical(CheckForInARow,CheckForInARow[1]:CheckForInARow[3])) {
      PlayerXScore = PlayerXScore + 3
      NoRunYet <- FALSE
    }
  }
  PegCallout <- ""
  if ((PlayerXScore-PointsGained)>0) {
    PegCallout <- paste("for",(PlayerXScore-PointsGained),sep=" ")
  }
  return(list(PlayerXScore,PeggingHand,tempStorage,runningCount,PegCallout))
}
DisplayCard <- function(card,case) {#Compact Card Display
  if (case==1) {
    colnames(card)[1] <- c("Starter") 
  } else if (case==2) {
    colnames(card)[1] <- c("Current Play") 
  } else if (case==3) {
    colnames(card)[1] <- c("Player 1's Hand") 
  } else if (case==4) {
    colnames(card)[1] <- c("Player 2's Hand") 
  } else if (case==5) {
    colnames(card)[1] <- c("Player 1's Crib")
  } else if (case==6) {
    colnames(card)[1] <- c("Player 2's Crib")
  } else if (case==7) {
    colnames(card)[1] <- c(" ")
  }
  card <- card[1]
  return(card)
}
