module Day7 where

import Control.Monad (replicateM)
import Data.List (elemIndices, find, sort)
import qualified Data.Map.Strict as Map
import Lib (countOccurrences, unwrap)

data HandType = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind deriving (Enum, Eq, Ord, Show)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Ord, Show)

charToCardImpl :: Char -> Card
charToCardImpl '2' = Two
charToCardImpl '3' = Three
charToCardImpl '4' = Four
charToCardImpl '5' = Five
charToCardImpl '6' = Six
charToCardImpl '7' = Seven
charToCardImpl '8' = Eight
charToCardImpl '9' = Nine
charToCardImpl 'T' = Ten
charToCardImpl 'Q' = Queen
charToCardImpl 'K' = King
charToCardImpl 'A' = Ace
charToCardImpl _ = error "Could not parse character as card"

charToCardJoker :: Char -> Card
charToCardJoker 'J' = Joker
charToCardJoker ch = charToCardImpl ch

charToCardJack :: Char -> Card
charToCardJack 'J' = Jack
charToCardJack ch = charToCardImpl ch

data Hand = Hand
  { handType :: HandType,
    -- has 5 cards always
    cards :: [Card]
  }
  deriving (Eq)

getHandType :: (Ord a) => [a] -> HandType
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

data JHand = JHand
  { jhandType :: HandType,
    -- has 5 cards always
    jcards :: [Card]
  }
  deriving (Eq, Ord)

instance Ord Hand where
  compare (Hand handType1 cards1) (Hand handType2 cards2)
    | handType1 == handType2 = compare cards1 cards2
    | otherwise = compare handType1 handType2

findJCard :: [(Int, Card)] -> Int -> Card
findJCard xs num = snd $ unwrap $ find ((== num) . fst) xs

-- | splits into the unparsed Card and the bid for that card
splitLine :: String -> (String, Int)
splitLine line = (head parts, read $ parts !! 1)
  where
    parts = words line

parseLineJack :: String -> (Hand, Int)
parseLineJack line =
  let (unparsedCard, bid) = splitLine line
      cards = map charToCardJack unparsedCard
      cardType = getHandType cards
      hand = Hand cardType cards
   in (hand, bid)

possibleJokerTransformations :: [Card]
possibleJokerTransformations = [Two .. Ten] ++ [Queen .. Ace]

parseLineJoker :: String -> (JHand, Int)
parseLineJoker line =
  let (unparsedCard, bid) = splitLine line
      originalCards = map charToCardJoker unparsedCard
      -- list of positions where Joker appears
      jokerAppears = elemIndices Joker originalCards
      -- all the possible permutations of the N amount of times that we see Joker card
      possibleJokerMorphs = map (zip jokerAppears) $ replicateM (length jokerAppears) possibleJokerTransformations

      possibleCards =
        map
          ( \jokerTransformation ->
              zipWith
                ( \originalCardIndex originalCard ->
                    ( if originalCard == Joker
                        then -- Joker transforms into a different card
                          findJCard jokerTransformation originalCardIndex
                        else originalCard
                    )
                )
                [0 ..]
                originalCards
          )
          possibleJokerMorphs
      bestCard = maximum (map getHandType possibleCards)
      hand = JHand bestCard originalCards
   in (hand, bid)

solve parseLine input =
  let parsed = map parseLine $ lines input
      sorted = map snd $ sort parsed
      totalWinnings = sum $ zipWith (*) sorted [1 ..]
   in show totalWinnings

part1 :: String -> String
part1 = solve parseLineJack

part2 :: String -> String
part2 = solve parseLineJoker
