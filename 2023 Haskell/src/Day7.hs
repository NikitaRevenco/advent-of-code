module Day7 where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Lib (countOccurrences)

data HandType = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind deriving (Enum, Eq, Ord, Show)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Ord, Show)

charToCard :: Char -> Card
charToCard '2' = Two
charToCard '3' = Three
charToCard '4' = Four
charToCard '5' = Five
charToCard '6' = Six
charToCard '7' = Seven
charToCard '8' = Eight
charToCard '9' = Nine
charToCard 'T' = Ten
charToCard 'J' = Jack
charToCard 'Q' = Queen
charToCard 'K' = King
charToCard 'A' = Ace
charToCard _ = error "Could not parse character as card"

getHandType :: [Card] -> HandType
getHandType cards
  -- All 5 cards have the same label
  | length occurences == 1 = FiveOfKind
  -- 4 cards share a label and 1 card has a different label
  | length occurences == 2 && (4 `elem` counts && 1 `elem` counts) =
      FourOfKind
  -- 3 cards share a label and 2 cards have a different label
  | length occurences == 2 && (2 `elem` counts && 3 `elem` counts) =
      FullHouse
  -- 3 cards share a label, 2 cards each have unique labels
  | length occurences == 3 && (3 `elem` counts) =
      ThreeOfKind
  -- 2 cards share a label, 2 cards share a different label, 1 card has a different label
  | length occurences == 3 && (1 `elem` counts && 2 `elem` counts) =
      TwoPair
  -- 2 cards share a label, 3 cards each have unique labels
  | length occurences == 4 && 2 `elem` counts =
      OnePair
  -- All 5 of the cards' labels are distinct
  | length occurences == 5 = HighCard
  | otherwise = error "Card does not match any pattern"
  where
    occurences = Map.toList $ countOccurrences cards
    counts = map snd occurences

data Hand = Hand
  { handType :: HandType,
    -- has 5 cards always
    cards :: [Card]
  }
  deriving (Eq)

instance Ord Hand where
  compare hand1@(Hand handType1 cards1) hand2@(Hand handType2 cards2)
    | handType1 == handType2 = compare cards1 cards2
    | otherwise = compare handType1 handType2

parseLine :: String -> (Hand, Int)
parseLine line =
  let both = words line
      bid' = read $ both !! 1
      cards = map charToCard $ head both
      cardType = getHandType cards
      hand = Hand cardType cards
   in (hand, bid')

part1 :: String -> String
part1 input =
  let sorted = map snd $ sort $ map parseLine $ lines input
      totalWinnings = sum $ zipWith (*) sorted [1 ..]
   in show totalWinnings
