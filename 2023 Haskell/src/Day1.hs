module Day1 where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

combineLeftRightDigits :: String -> Int
combineLeftRightDigits input = read [head digits, last digits]
  where
    digits = filter isDigit input

part1 :: String -> String
part1 = show . sum . map combineLeftRightDigits . lines

numbers =
  [ ("nine", "9"),
    ("eight", "8"),
    ("seven", "7"),
    ("six", "6"),
    ("five", "5"),
    ("four", "4"),
    ("three", "3"),
    ("two", "2"),
    ("one", "1"),
    ("zero", "0")
  ]

replaceWordsWithNumbers :: [(String, String)] -> String -> String
replaceWordsWithNumbers [] (firstChar : restOfChars) = firstChar : replaceWordsWithNumbers numbers restOfChars
replaceWordsWithNumbers ((word, digit) : otherDigits) text
  | word `isPrefixOf` text =
      replaceWordsWithNumbers otherDigits $ digit ++ drop 1 text
  | otherwise =
      replaceWordsWithNumbers otherDigits text
replaceWordsWithNumbers _ oneChar = oneChar

part2 :: String -> String
part2 = show . sum . map (combineLeftRightDigits . replaceWordsWithNumbers numbers) . lines
