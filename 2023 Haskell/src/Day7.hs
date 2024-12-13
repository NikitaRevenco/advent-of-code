module Day7 where

import Control.Monad (replicateM)
import Data.List (elemIndices, find, sort)
import qualified Data.Map.Strict as Map
import Lib (countOccurrences, unwrap)

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

data Hand = Hand
  { handType :: HandType,
    -- has 5 cards always
    cards :: [Card]
  }
  deriving (Eq)

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
  let parsed = map parseLine $ lines input
      sorted = map snd $ sort parsed
      totalWinnings = sum $ zipWith (*) sorted [1 ..]
   in show totalWinnings

getHandTypeJ :: [JCard] -> HandType
getHandTypeJ cards
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

data JHand = JHand
  { jhandType :: HandType,
    -- has 5 cards always
    jcards :: [JCard]
  }
  deriving (Eq, Ord)

instance Ord Hand where
  compare hand1@(Hand handType1 cards1) hand2@(Hand handType2 cards2)
    | handType1 == handType2 = compare cards1 cards2
    | otherwise = compare handType1 handType2

data JCard = Joker | JTwo | JThree | JFour | JFive | JSix | JSeven | JEight | JNine | JTen | JQueen | JKing | JAce deriving (Enum, Eq, Ord, Show)

charToJCard :: Char -> JCard
charToJCard '2' = JTwo
charToJCard '3' = JThree
charToJCard '4' = JFour
charToJCard '5' = JFive
charToJCard '6' = JSix
charToJCard '7' = JSeven
charToJCard '8' = JEight
charToJCard '9' = JNine
charToJCard 'T' = JTen
charToJCard 'J' = Joker
charToJCard 'Q' = JQueen
charToJCard 'K' = JKing
charToJCard 'A' = JAce
charToJCard _ = error "Could not parse character as card"

resolveJokers :: [JCard] -> [[JCard]]
resolveJokers = pure

jokerMorphs = [JTwo .. JAce]

findJCard :: [(Int, JCard)] -> Int -> JCard
findJCard xs num = snd $ unwrap $ find ((== num) . fst) xs

parseLineJ :: String -> (JHand, Int)
parseLineJ line =
  let both = words line
      bid' = read $ both !! 1
      originalCards = map charToJCard $ head both
      -- list of positions where Joker appears
      jokerAppears = elemIndices Joker originalCards
      -- all the possible permutations of the N amount of times that we see Joker card
      possibleJokerMorphs = map (zip jokerAppears) $ replicateM (length jokerAppears) jokerMorphs
      possibleCards =
        map
          ( \jokerTransformation ->
              zipWith
                ( \originalCardIndex originalCard ->
                    ( if originalCard == Joker
                        then findJCard jokerTransformation originalCardIndex
                        else originalCard
                    )
                )
                [0 ..]
                originalCards
          )
          possibleJokerMorphs
      bestCard = maximum (map getHandTypeJ possibleCards)
      hand = JHand bestCard originalCards
   in (hand, bid')

part2 :: String -> String
part2 input =
  let parsed = map parseLineJ $ lines input
      sorted = map snd $ sort parsed
      totalWinnings = sum $ zipWith (*) sorted [1 ..]
   in show totalWinnings
