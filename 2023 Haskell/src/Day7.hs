{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Day7 where

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
  | length occurences == 1 = FiveOfKind
  | length occurences == 5 = HighCard
  | length occurences == 2
      && ( (first == 4 && second == 1)
             || (first == 1 && second == 4)
         ) =
      FourOfKind
  | length occurences == 2
      && ( (first == 3 && second == 2)
             || (first == 2 && second == 3)
         ) =
      FullHouse
  | length occurences == 3
      && ( (first == 3)
             || (second == 3)
             || (third == 3)
         ) =
      ThreeOfKind
  | length occurences == 3
      && ( (first == 2 && second == 2)
             || (first == 2 && third == 2)
             || (second == 2 && third == 2)
         ) =
      TwoPair
  | length occurences == 4
      && elem 2 (map snd occurences) =
      OnePair
  | otherwise = error "Card does not match any pattern"
  where
    occurences = Map.toList $ countOccurrences cards
    first = snd (head occurences)
    second = snd (occurences !! 1)
    third = snd (occurences !! 2)

data Hand = Hand
  { handType :: HandType,
    -- has 5 cards always
    cards :: [Card]
  }

compareHands :: Hand -> Hand -> Hand
compareHands hand1@(Hand handType1 cards1) hand2@(Hand handType2 cards2)
  | handType1 == handType2 =
      if handType1 > handType2
        then hand1
        else hand2
  | cards1 > cards2 = hand1
  | otherwise = hand2

parseLine :: String -> ([Card], Int)
parseLine line =
  let both = words line
      bid' = read (both !! 1) :: Int
      cards = map charToCard (both !! 0)
   in (cards, bid')

-- parseLine :: String ->

-- part1 :: String -> String
-- part1 input = map parseLine $ lines input

-- part2 :: String -> String
-- part2 = id
