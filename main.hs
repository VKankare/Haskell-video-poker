import System.Random
import Data.List

--define deck suits
data CardSuit = Club | Diamond | Heart | Spade
  deriving (Read, Show, Enum, Eq, Ord)

--define card values
data CardValue =  Two | Three | Four | Five | Six
                | Seven | Eight | Nine | Ten
                | Jack | Queen | King | Ace
  deriving (Read, Show, Enum, Eq, Ord)

--define what a card is
data Card = Card {value :: CardValue,
                  suit :: CardSuit}
  deriving (Show, Eq)

--create a deck
type Deck = [Card]

--put cards in deck
deck :: Deck
deck = [Card value suit | value <- [Two .. Ace], suit <- [Club .. Spade]]

--shuffle deck with Fisher-Yates algorithm
shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    shuffle deck
  where
    shuffle [] = return []
    shuffle deck = do
      randomIndex <- getStdRandom $ randomR (0, length deck - 1)
      let (left, chosenCard:right) = splitAt randomIndex deck
      rest <- shuffle $ left ++ right
      return $ chosenCard : rest

dealCard :: Deck -> (Card, Deck)
dealCard (card:deck) = (card, deck)

--loop through hand and ask which cards user wants to discard
discardCards :: Int -> [Card] -> IO [Card]
discardCards num hand = do
    if num < length hand
    then do
        putStrLn ("Current hand: " ++ show hand)
        putStrLn ("Discard " ++ show (hand !! num) ++ "? y/n")
        question <- getLine

        if question == "y"
        then do
          let newHand = delete (hand !! num) hand
          discardCards num newHand
        else if question == "n"
        then do
          discardCards (num + 1) hand
        else do
          putStrLn "\ninvalid input, input either y or n"
          discardCards num hand
    else do
      return hand

--deal five cards from the previously shuffled deck and return a hand
dealNewHand :: [Card] -> Deck -> IO [Card]
dealNewHand hand deck = do
  if length hand < 5
  then do
    let (card, remainingDeck) = dealCard deck
    let newHand = hand ++ [card]
    dealNewHand newHand remainingDeck
  else return hand

--remove first five cards of the deck and return the deck
removeFirstFive :: Int -> Deck -> IO Deck
removeFirstFive num deck = do 
  if num < 5
  then do 
    let (card, remainingDeck) = dealCard deck
    removeFirstFive (num + 1) remainingDeck
  else return deck

main :: IO ()
main = do
  putStrLn "\n***Haskell Video Poker by Viljo Kankare***\n"

  --shuffle deck, deal hand
  shuffledDeck <- shuffleDeck deck
  let firstDeal = []
  startingHand <- dealNewHand firstDeal shuffledDeck
  newDeck <- removeFirstFive 0 shuffledDeck

  --start discard phase
  putStrLn "Discard up to 5 cards\n"
  newHand <- discardCards 0 startingHand

  --deal final cards
  finalHand <- dealNewHand newHand newDeck
  putStrLn ("\nYour final hand: " ++ show finalHand)