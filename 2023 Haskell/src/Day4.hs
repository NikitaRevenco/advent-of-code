module Day4 where

import Control.Arrow (Arrow ((***)))
import Control.Monad (join)
import Data.List (elemIndex)
import Debug.Trace (traceShowId)
import Lib (unwrap)

cardPoints :: String -> Int
cardPoints card =
  let colonIndex = unwrap $ ':' `elemIndex` card
      -- +1 to also remove the colon
      restOfCard = drop (colonIndex + 1) card
      barIndex = unwrap $ '|' `elemIndex` restOfCard
      (winningNumbers, currentNumbers) = join (***) words (barIndex `splitAt` restOfCard)
      myWinningNumbers = length $ filter (`elem` winningNumbers) (drop 1 currentNumbers)
   in traceShowId $ (if myWinningNumbers == 0 then 0 else (2 ^ (myWinningNumbers - 1)))

part1 :: String -> String
part1 input = show $ sum $ map cardPoints $ lines input

part2 :: String -> String
part2 = id
