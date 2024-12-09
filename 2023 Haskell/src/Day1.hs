module Day1 where

import Data.Char (isDigit)
import Data.List (find)
import Lib (unwrap)

scanNumber :: String -> Int
scanNumber input = read [firstDigit, lastDigit]
  where
    firstDigit = unwrap $ find isDigit input
    lastDigit = unwrap $ find isDigit $ reverse input

part1 :: String -> String
part1 input = show lala
  where
    lmao = map scanNumber $ lines input
    lala = sum lmao
