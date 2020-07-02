module A = Belt.Array;
Js.log("Time to play some Euchre");
let seed = int_of_float(Js.Date.now());
Random.init(seed);

// ♠ ♥ ♦ ♣ ♤ ♡ ♢ ♧
type suit = Hearts | Diamonds | Spades | Clubs;
type card = {
    suit: suit,
    value: int
}
type deck = {
  cards: list(card),
}
type hand = | Empty | Cards(array(card));
type playedCard = | None | Card(card);
type ledSuit = | NoSuit | Suit(suit);
type player = {
  name: string,
  position: int,
  mutable hand: hand,
}

let addToHand = (myPlayer, myCard) => {
  switch (myPlayer.hand) {
    | Empty => Cards([| myCard |])
    | Cards(arr) => Cards(Array.append(arr, [|myCard|]))
  }
}
let getFromHand = (myPlayer, cardPos) => {
  switch (myPlayer.hand) {
    | Empty => None
    | Cards(arr) => cardPos < Array.length(arr) ? Card(arr[cardPos]) : None
  }
}
let removeFromHand = (myPlayer, cardPos) => {
  switch (myPlayer.hand) {
    | Empty => Cards([||])
    | Cards(arr) => cardPos < Array.length(arr) ? Cards(Js.Array.spliceInPlace(~pos=cardPos, ~remove=1, ~add=[||], arr)) : Cards([||])
  }
}
let playFromHand = (myPlayer, cardPos) => {
  let cardToPlay = getFromHand(myPlayer, cardPos)
  removeFromHand(myPlayer, cardPos)
  cardToPlay
}

let isRightBower = (trump: suit, card: card) => {
  switch(trump) {
    | Hearts => card.suit == Hearts && card.value == 11
    | Diamonds => card.suit == Diamonds && card.value == 11
    | Spades => card.suit == Spades && card.value == 11
    | Clubs => card.suit == Clubs && card.value == 11
  }
}
let isLeftBower = (trump: suit, card: card) => {
  switch(trump) {
    | Hearts => card.suit == Diamonds && card.value == 11
    | Diamonds => card.suit == Hearts && card.value == 11
    | Spades => card.suit == Clubs && card.value == 11
    | Clubs => card.suit == Spades && card.value == 11
  }
}
let isTrump = (trump: suit, card: card) => {
  card.suit == trump || isLeftBower(trump, card)
}
let getValue = (trump: suit, card: card) => {
  if (isRightBower(trump, card)) {
    16
  } else if (isLeftBower(trump, card)) {
    15
  } else {
    card.value
  }
}

// A > B -> 1; A < B -> -1; A == B -> 0
let compareCards = (trump: suit, led: ledSuit, cardA: card, cardB: card) => {
  if (isTrump(trump, cardA) || isTrump(trump, cardB)) {
    getValue(trump, cardA) - getValue(trump, cardB)
  } else {
    if(cardA.suit == cardB.suit) {
      getValue(trump, cardA) - getValue(trump, cardB)
    } else {
      switch(led) {
        | NoSuit => 0
        | Suit(suit) => {
          if(cardA.suit == suit) {
            1
          } else if (cardB.suit == suit) {
            -1
          } else {
            0
          }
        }
      }
    }
  }
}
// Curry the comparison function so we can pass it into the array sort method
let getCompFn = (trump: suit, led: ledSuit) => {
  compareCards(trump, led)
}
let sortHand = (myHand: hand, trump: suit, led: ledSuit) => {
  switch(myHand) {
    | Empty => Empty
    | Cards(arr) => {
      Array.stable_sort(getCompFn(trump, led), arr);
      Cards(arr)
    }
  }
}

// Returns index in players hand of highest trump card
let getHighestTrump = (myHand: hand, trump: suit) => {
  switch (myHand) {
    | Empty => -1
    | Cards(arr) => {
      let maxPos = ref(-1);
      let maxVal = ref(-1);
      for (idx in 0 to Array.length(arr) - 1) {
        let cardVal = getValue(trump, arr[idx]);
        if(isTrump(trump, arr[idx]) && cardVal > maxVal^) {
          maxVal := cardVal;
          maxPos := idx;
        }
      };
      maxPos^
    }
  }
}
// Returns the index of highest led suit card
let getHighestLed = (myHand: hand, trump: suit, led: ledSuit) => {
  switch (myHand) {
    | Empty => -1
    | Cards(arr) => {
      switch (led) {
        | NoSuit => -1
        | Suit(suit) => {
          let maxPos = ref(-1);
          let maxVal = ref(-1);

          for (idx in 0 to Array.length(arr) - 1) {
            let cardVal = getValue(trump, arr[idx]);
            if(arr[idx].suit == suit && cardVal > maxVal^) {
              maxVal := cardVal;
              maxPos := idx;
            }
          };
          maxPos^
        }
      }
    }
  }
}
// returns index of highest non-trump card
let getHighestNonTrump = (myHand: hand, trump: suit) => {
  switch (myHand) {
    | Empty => -1
    | Cards(arr) => {
      let maxPos = ref(-1);
      let maxVal = ref(-1);
      for (idx in 0 to Array.length(arr) -1) {
        let cardVal = getValue(trump, arr[idx]);
        if(!isTrump(trump, arr[idx]) && cardVal > maxVal^) {
          maxVal := cardVal;
          maxPos := idx;
        }
      };
      maxPos^
    }
  }
}
// getLowestNonTrumpNonLed =>

let getSuitString = (mySuit) => {
  switch (mySuit) {
    | Hearts => {j|♥ |j}
    | Diamonds => {j|♦ |j}
    | Spades => {j|♠ |j}
    | Clubs => {j|♣ |j}
  }
}

let getValueString = (myValue) => {
  switch (myValue) {
    | 9 => "Nine"
    | 10 => "Ten"
    | 11 => "Jack"
    | 12 => "Queen"
    | 13 => "King"
    | 14 => "Ace"
    | 15 => "Left Bower" // Probably not going to keep this abstraction
    | 16 => "Right Bower"
    | _ => "You can't play a Get Out Of Jail Free Card in Euchre!"
  }
}

let cardToString = (myCard) => getValueString(myCard.value) ++ " of " ++ getSuitString(myCard.suit);
let playedCardToString = (myPlayedCard) => {
  switch (myPlayedCard) {
    | None => "Not a card"
    | Card(card) => cardToString(card)
  }
}
let playedCardsToString = arrOfPlayedCards => Js.Array.joinWith(", ", Array.map(pc => playedCardToString(pc), arrOfPlayedCards));
let playerHandToString = (myHand) => {
  switch (myHand) {
    | Empty => "0 Cards"
    | Cards(arr) => Js.Array.joinWith(", ", Array.map(c => cardToString(c), arr))
  }
}

let getPlayerName = (myPlayer) => { "Player " ++ myPlayer.name }
let playerToString = (myPlayer) => { "Player " ++ myPlayer.name ++ " has a hand of: " ++ playerHandToString(myPlayer.hand); }
let printDeck = (myDeck) => { myDeck |> List.map(c => Js.log(cardToString(c))); }
let printPlayers = (players) => { players |> Array.map(p => Js.log(playerToString(p))); }

let shuffleDeck = (myDeck) => {
  // There are only 24 cards in a euchre deck.
  let newpositions = Belt.Array.shuffle([|0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23|])
  Array.to_list(Array.map(p => List.nth(myDeck, p), newpositions))
}

let dealHand = (shuffledDeck, players) => {
  /*
    It is customary in euchre not to deal out all 5 cards at once, rather alternating 3 or 2 cards alternating to each player at the table, then
    2 or 3 to each player once again for a total of 5 per player. There is some debate about whether you should start by dealing 3 first or 2 first,
    either giving your opponents or your teammate more cards from the top of the deck. There is an alternate deal order provided.
  */
  let dealOrder = [(0, 0), (1, 0), (2, 0), (3, 1), (4, 1), (5, 2), (6, 2), (7, 2), (8, 3), (9, 3), (10, 0), (11, 0), (12, 1), (13, 1), (14, 1), (15, 2), (15, 2), (16, 3), (17, 3), (18, 3),];
  // let dealOrder = [(0, 0), (1, 0), (2, 1), (3, 1), (4, 1), (5, 2), (6, 2), (7, 3), (8, 3), (9, 3), (10, 0), (11, 0), (12, 0), (13, 1), (14, 1), (15, 2), (15, 2), (16, 2), (17, 3), (18, 3),];
  // let flippedCardPos = 19;

  List.iter((dealTuple) => {
    let (cardPos, playerPos) = dealTuple;
    let drawOne = List.nth(shuffledDeck, cardPos);
    players[playerPos].hand = addToHand(players[playerPos], drawOne)
  }, dealOrder);
}

// A & C are partners, as are B & D
let players = [|{name: "A", position: 0, hand: Empty}, {name: "B", position: 1, hand: Empty}, {name: "C", position: 2, hand: Empty}, {name: "D", position: 3, hand: Empty}|]
let teamA = 0;
let teamB = 1;
let teamAScore = ref(0);
let teamBScore = ref(0);
let deck = [9,10,11,12,13,14] |> List.map(n => [{suit: Spades, value: n}, {suit: Hearts, value: n}, {suit: Diamonds, value: n}, {suit: Clubs, value: n}]) |> List.flatten;


/************************************************************/
/******************  Lets play the game!  *******************/
/************************************************************/

// Game loop
// While(neither team has hit 10 points):
  // determineDealer => (lastDealer + 1 % 4) or 0 if no last player
  // playARound
    // Deal cards starting with the dealer
    // Flip card
    // choose trump
    // Play tricks
    // for i = 0; i < 5; i++:
      // determineLeadPlayer = if lastLeadPlayer is null (then dealer + 1 => person to left of dealer)
      // each player plays one card (determine best card to play)
      // Determine winner

// congratulateWinner(teamWithMostPoints)

let getPositionOfHighestCard = (playedCards: array(playedCard), trump: suit, ledCard: playedCard) => {
  let led = switch (ledCard) {
    | None => NoSuit
    | Card(card) => {
      Js.log(getSuitString(card.suit) ++ " was led")
      Suit(card.suit)
    }
  }
  // Pretend the played cards are a player's hand so we can use our existing hand rating functions
  let tempPlayer = {name: "TEMP", position: 99, hand: Empty};
  for (idx in 0 to Array.length(playedCards) - 1) {
    switch (playedCards[idx]) {
      | None => Empty
      | Card(card) => {
        tempPlayer.hand = addToHand(tempPlayer, card);
        tempPlayer.hand
      }
    }
  };
  let positionOfHighestTrump = getHighestTrump(tempPlayer.hand, trump);
  if(positionOfHighestTrump >= 0) {
    positionOfHighestTrump
  } else {
    let positionOfHighestLed = getHighestLed(tempPlayer.hand, trump, led);
    positionOfHighestLed
  }
}

// This version of play card will follow the rules but will play naïvely.
let playCard = (player, trump, playedCards, ledCard) => {
  let ledSuit = switch (ledCard) {
    | None => NoSuit
    | Card(card) => Suit(card.suit)
  }

  if (ledSuit == NoSuit) {
    let positionOfHighestNonTrump = getHighestNonTrump(player.hand, trump);
    if(positionOfHighestNonTrump >= 0) {
      playFromHand(player, positionOfHighestNonTrump);
    } else {
      let positionOfHighestTrump = getHighestTrump(player.hand, trump);
      playFromHand(player, positionOfHighestTrump);
    }
  } else {
    let positionOfHighestLedSuit = getHighestLed(player.hand, trump, ledSuit);
    if(positionOfHighestLedSuit >= 0) {
      playFromHand(player, positionOfHighestLedSuit);
    } else {
      let positionOfHighestTrump = getHighestTrump(player.hand, trump);
      if(positionOfHighestTrump >= 0) {
        playFromHand(player, positionOfHighestTrump);
      } else {
        let positionOfHighestNonTrump = getHighestNonTrump(player.hand, trump);
        playFromHand(player, positionOfHighestNonTrump);
      }
    }
  }
}

let playTrick = (players, trump, leader) => {
  Js.log("Playing trick - " ++ getSuitString(trump) ++ " are trump")
  let playOrder = [|leader, (leader + 1) mod 4, (leader + 2) mod 4, (leader + 3) mod 4|];
  let ledCard = None
  let playedCards = [||]

  let playedCards = Array.append(playedCards, [|playCard(players[playOrder[0]], trump, playedCards, ledCard)|]);
  let ledCard = playedCards[0];
  let playedCards = Array.append(playedCards, [|playCard(players[playOrder[1]], trump, playedCards, ledCard)|]);
  let playedCards = Array.append(playedCards, [|playCard(players[playOrder[2]], trump, playedCards, ledCard)|]);
  let playedCards = Array.append(playedCards, [|playCard(players[playOrder[3]], trump, playedCards, ledCard)|]);

  Js.log(playedCardsToString(playedCards))
  let winningCardPos = getPositionOfHighestCard(playedCards, trump, ledCard);

  Js.log("Winning card: " ++ playedCardToString(playedCards[winningCardPos]))
  let winningPlayer = playOrder[winningCardPos];
  winningPlayer
}

let scoreTrick = (teamThatCalledTrump, teamATricks, teamBTricks, teamAWentAlone, teamBWentAlone) => {
  if (teamThatCalledTrump == 0) {
    if (teamAWentAlone) {
      switch (teamATricks, teamBTricks) {
        | (5, 0) => (teamA, 4)
        | (3 | 4, 1 | 2) => (teamA, 1)
        | (0 | 1 | 2, 3 | 4 | 5) => (teamB, 2) // Euchred!
      }
    } else {
      switch (teamATricks, teamBTricks) {
        | (5, 0) => (teamA, 2)
        | (3 | 4, 1 | 2) => (teamA, 1)
        | (0 | 1 | 2, 3 | 4 | 5) => (teamB, 2) // Euchred!
      }
    }
  } else {
    if(teamBWentAlone) {
      switch (teamBTricks, teamATricks) {
        | (5, 0) => (teamB, 4)
        | (3 | 4, 1 | 2) => (teamB, 1)
        | (0 | 1 | 2, 3 | 4 | 5) => (teamA, 2) // Euchred!
      }
    } else {
      switch (teamBTricks, teamATricks) {
        | (5, 0) => (teamB, 2)
        | (3 | 4, 1 | 2) => (teamB, 1)
        | (0 | 1 | 2, 3 | 4 | 5) => (teamA, 2) // Euchred!
      }
    }
  }
}

// Returns a tuple containing the winning team (0=A, 1=B) and the points that team earned.
let playTurn = (players, shuffled) => {
  let dealer = 0; // TODO determineDealer => (lastDealer + 1 % 4) or 0 if no last player
  dealHand(shuffled, players); // TODO start from the dealer or reorder players?
  let trump = Hearts; // TODO add subroutine for calling trump
  let teamThatCalledTrump = 0; // TODO - save the team that called trump
  let teamATricks = ref(0);
  let teamBTricks = ref(0);

  Js.log("Playing round with players:");
  for (playerNum in 0 to 3) {
    sortHand(players[playerNum].hand, trump, NoSuit)
  }
  printPlayers(players);
  Js.log("\n")

  for (roundNum in 0 to 4) {
    let leader = dealer; // TODO determine lead player
    let winningPlayer = playTrick(players, trump, leader);

    // 0 & 2 are team A, 1 & 3 are team B
    winningPlayer mod 2 == 0 ? teamATricks := teamATricks^ + 1 : teamBTricks := teamBTricks^ + 1;
    Js.log("Winning Player: " ++ getPlayerName(players[winningPlayer]))
    Js.log("\n");
  };
  // Js.log("Finished round");
  // Js.log("Team a tricks:")
  // Js.log(teamATricks^)
  // Js.log("Team b tricks:")
  // Js.log(teamBTricks^)

  scoreTrick(teamThatCalledTrump, teamATricks^, teamBTricks^, false, false);
}

let playGame = (players, deck) => {
  while(teamAScore^ < 10 && teamBScore^ < 10) {
    Js.log("Team A points: ")
    Js.log(teamAScore^)
    Js.log("Team B points: ")
    Js.log(teamBScore^)
    let (teamThatWonTrick, score) = playTurn(players, shuffleDeck(deck));
    teamThatWonTrick == teamA ? teamAScore := teamAScore^ + score : teamBScore := teamBScore^ + score;
  }

  if(teamAScore^ >= 10) {
    Js.log("Congratulations to team A!")
  } else {
    Js.log("Congratulations to team B!")
  }
}
playGame(players, deck)

// We can now play a full game! (Albeit naïvely)
// TODO
// Game concepts: called suit, better tactics, going alone
// Stats concepts: Who won a game, who won each round, what was trump, who won each round with what hands
// What questions are we trying to answer?
/*
- Under what circumstances should we call a given suit for trump?
- Under what circumstances should we go alone?
- What are the best tactics to play?
*/