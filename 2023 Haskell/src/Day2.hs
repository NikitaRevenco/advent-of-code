module Day2 where

import Data.List (elemIndex)
import Lib (slice, splitByCh, unwrap, unwrapOr)

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
parseRound gameRound =
  Round
    { red = getColor "red",
      green = getColor "green",
      blue = getColor "blue"
    }
  where
    lamb [count, color] = (color, read count)
    lamb _ = error "Non-exhaustive pattern matching"
    colors = map (lamb . words) $ splitByCh ',' gameRound
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
isValidRound gameRound =
  red gameRound <= 12
    && green gameRound <= 13
    && blue gameRound <= 14

isValidGame :: Game -> Bool
isValidGame game = all isValidRound $ rounds game

part1 :: String -> String
part1 = show . sum . map gameId . filter isValidGame . map parseGame . lines

maxOf :: Round -> Round -> Round
maxOf (Round r1 g1 b1) (Round r2 g2 b2) = Round (max r1 r2) (max g1 g2) (max b1 b2)

-- | A round with the fewest number of cubes required to play the entire game
fewestCubes :: Game -> Round
fewestCubes game = foldl maxOf (Round 0 0 0) $ rounds game

power :: Round -> Int
power (Round r g b) = r * g * b

part2 :: String -> String
part2 = show . sum . map (power . fewestCubes . parseGame) . lines
