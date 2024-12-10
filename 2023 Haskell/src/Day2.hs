module Day2 where

import Data.List (elemIndex)
import Lib (slice, splitByCh, trim, unwrap, unwrapOr)

data Round = Round
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

data Game = Game
  { gameId :: Int,
    rounds :: [Round]
  }
  deriving (Show)

{-
Each round is of the form
1 blue, 2 green
-}
parseRound :: String -> Round
parseRound round =
  Round
    { red = getColor "red",
      green = getColor "green",
      blue = getColor "blue"
    }
  where
    lamb [count, color] = (color, read count :: Int)
    colors = map (lamb . words) $ splitByCh ',' round
    getColor color = lookup color colors `unwrapOr` 0

{-
Each game is of the form
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue

5 to index of ":"
-}
parseGame :: String -> Game
parseGame input =
  Game
    { gameId = read $ slice 5 colonIndex input,
      rounds = map parseRound $ splitByCh ';' $ drop (colonIndex + 1) input
    }
  where
    colonIndex = unwrap $ elemIndex ':' input

isValidRound :: Round -> Bool
isValidRound round =
  red round <= 12
    && green round <= 13
    && blue round <= 14

isValidGame :: Game -> Bool
isValidGame game = all isValidRound $ rounds game

part1 :: String -> String
part1 str = show . sum $ map gameId $ filter isValidGame $ map parseGame (lines str)
