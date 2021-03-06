This is my first self-imposed long-term programming project in order to practice coding. In the future I plan to add more elements to this code, such as making the opponent "smarter", compatibility with 3/4 player games, and graphical analyzation over a large set of games.

# Cribbage
This set of functions will provide interactive and simulated cribbage games for 2 players. The opponent for the interactive function as well as both players in the simulated games will choose cards at random, without assessing the optimal play in the game.

## Part 1: CribbageSetup

There are 4 functions in this set. This first function, CribbageSetup, is needed to run any of the 3 functions. It contains all the scoring mechanisms, prompting players, and announcement text that is required to play cribbage.

It includes:

  -Creating a deck of cards

  -Counting a hand's points in the show for 15's, sequences, pairs, flushes, and the right jack.

  -Combining all these show points together in a text format

  -Checking if a player has won

  -Mechanism for a player to choose which cards to either put away in the crib or use in the play

  -Checking the current play/count to see if any points are awarded

  -Some display information to make the game easier to read

## Part 2: PlayCribbage

This function is a player versus a randomized opponent. Any time a player must make a choice of cards, such as what to discard or what to play in the pegging, this function will prompt the player for each choice. The opponent's choices will be completely randomized, playing a random card as long as it is a legal move. Informative text that display how points are awarded and the hands that those points came from are also printed each turn, to make sure the player can make accurate decisions.

## Part 3: SimCribbage

This function simulates a whole game of cribbage by having both players play completely random cards. Informative text is still shown describing points awarded along with the hands where these points come from. At the end of the game, the scores of both of the players are outputted in a list.

## Part 4: SimCribbageNoTxt

This function simulates a whole game of cribbage with randomized plays, just like SimCribbage. This function, however, removes all the printed text and only outputs the scores of both players in a list. This function is meant to be used when many games have to be simulated, ideally for getting specific data from random games of cribbage. This still uses CribbageSetup, where there is code that saves strings of characters that tell information about what is going on, but these strings won't be displayed.

## Part 5: The Rules of Cribbage

Cribbage is usually played by 2,3, or 4 players, though this release only allows for 2. The object of the game is to reach 121 points first. When calculating wins of a game, extra points may be awarded based on how much a player won by. If a player gets to 121 before their opponent has reached 91 points (or 3/4 of the way) then the losing player was "skunked." If a player reaches 121 before their opponent has reached 61 points (1/2 of the way) then the winner has "double-skunked" their opponent. Rules of how many additional "games" have been won after skunking or double-skunking vary.

### Section a: Dealing Cards

Both players pick a card from the deck, whoever has the lowest card has the crib first. The "crib" is an additional way for a player to earn more points, and since there are only 2 players, the crib alternates between the two after each round. Each player gets 6 cards, and must choose 2 cards to be put in the crib. This means that the person without the crib has to give up two of their cards to the other player, while the player with the crib must put aside 2 cards that will be counted later. After this decision has been made, there are now 3 piles. Player 1's hand of 4 cards, player 2's hand of 4 cards, and the crib of 4 cards. These sets of cards will be used to determine how many points each player gets. Again, the player with the crib will get points both from his hand and from the crib, while the person without the crib only gets points using his own hand.

Cards are classified in similar ways as other card games. For example, the suit of the card is a factor in some of the scoring opportunities. The value of a card is the number on the card, so a 3 has a value of 3, a 7 has a value of 7, etc. Aces have a value of 1, and the Jack, Queen, and King all have values of 10. The order of a card, however, stays the same and is used in some instances of scoring opportunities. The order of cards always goes A,2,3,4,5,6,7,8,9,10,J,Q,K. However, when a card's value is used in scoring, those substitutions are made.

Before the play begins, an additional card, called the "Starter Card," is turned over. This card, after the play, can be used when tallying up points. So, when each player tallies up points, either from their hand or from the crib, they will actually be calculating their scores based on 5 cards- 4 from their set of cards plus the starter. The important part here is that neither player knows what the starter card will be until after they gave up cards for the crib. An additional rule here is that if the card that is turned over is a Jack, the player with the crib earns 2 points automatically.

### Section b: The Play

Before tallying the points from the players' respective hands, they will reveal their cards sequentially with an opportunity to earn additional points (Note that the Crib is not involved in this process at all). The player without the crib will begin by placing down a card, announcing its value. The next player will place down one of their cards, annoucning the total value on the board. So, for example, a player could put down a 7 and announce "7". The next player could place down an ace and announce "8". This value can never exceed 31 in the play. If a player cannot place down a card that will keep the count under 32, they must pass their turn, usually by saying "Go". If the other player can place down a card and still keep it under 32, they must do so. This will continue until that player cannot place down a card and keep it under 32. Once this happens, that player will receive 1 point. The person that originally passed will start the next count, starting back at 0, and the placing down of cards will alternate between them again. When a player runs out of their hand, it's an automatic "Go", with the other player placing down the rest of their cards as long as it is under 32. If the player can't stay below 32, they award themselves a point and start the next count themselves. Whoever places down the last card always gets 1 point for placing down the last card in the play.

During this process a number of things can happen that award the players points. If a player places down a card that gets the count to 31, the player gets 2 points (the player only gets 2 points here, they do not get 2 points followed by 1 point for a "Go"). The same happens with 15. The player that places down a card that gets the count to 15 gets 2 points. If a player plays a card with the same order as the previous card, it is a pair for 2 points. If another card is played with the same order, it is 3 of a kind for 6 points (cards ABC, 1 pair of AB, 1 for BC, 1 for AC). 4 of a kind is 12 points. If 3 cards are consecutively played that align in a row (for example, 9,10,J) then the player that placed the last card gets 3 points. Similarly, 4 points are for 4 in a row, 5 for 5 in a row, 6 for 6 in a row, and 7 points for 7 cards in a row. Note that the cards do not have to be placed in order for this to happen, for example, a sequence of 4,7,6,5 will award 4 points to the player that last placed down the 5, but not 3 points to the player that placed the 6 since 4,6,7 is not a sequence. However, the sequence must be within the same count. So if a count was finished with a 7 for 29 points, and the count reset, playing a 6 then a 5 will not award the sequence points. If the count is reset, the cards that are candidates for the sequences and pairs are also reset.

### Section c: The show

Each player now tallies up the scores from their respective hands. The player without the crib goes first in counting and applying their points. Once again, they have their 4 cards plus the starter card that can award them points. The cards in their hand can be used more than once for different point combinations (for example the 3 of a kind for 6 was just 3 different pairs of 2. Another example of this is a 4,5,6 which gives a value of 15 cummulative points as well as a run of 3 in a row). If any combination of cards add up to 15, the player gets 2 points. Any sequence also gets its length in points; a 3 in a row is worth 3 points, a 5 in a row is worth 5, etc. Pairs are worth 2, 3 of a kind is worth 6, and 4 of a kind is worth 12. If a player's hand all has cards of the same suit, or a flush, it is worth 4, and if the starter card also has that suit, it is worth 5. Finally, if a player has a Jack and the suit of the Jack matches the suit of the starter card, the player gets 1 point. Tallying up points would typically look something like this: "15 for 2, 15 for 4, 3 in a row is 7, and the right jack for 8". Once the player without the crib goes, now the player with the crib tallies up their hand plus the starter card. Then, the player now tallies up their points for the crib plus the starter. The scoring is the exact same with the exception of flushes. All 5 cards-the crib cards and the starter, must be the same suit in order for the player to receive points based on a flush. Once this happens, the player without the crib now will get the crib in the next round, and the process repeats. Points are applied immediately after earning, so as soon as one player gets to 121, they win.
