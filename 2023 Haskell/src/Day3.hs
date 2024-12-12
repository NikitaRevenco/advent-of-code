module Day3 where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Lib (enumerate, maybeAt, slice)

-- | Whether a character is considered a symbol
--
-- # Examples
--
-- >>> isSymbol '#'
-- True
--
-- >>> isSymbol '.'
-- False
--
-- >>> isSymbol '4'
-- False
isSymbol :: Char -> Bool
isSymbol ch = not (isDigit ch) && ch /= '.'

-- | (to, from) corresponding to the inclusive range that numbers span
type NumberRange = (Int, Int)

-- | (line number, to, from)
type LinenrNumberRange = (Int, Int, Int)

numberParser :: [NumberRange] -> (Int, Char) -> [NumberRange]
numberParser acc (i, ch)
  | isDigit ch && null acc = [(i, i)]
  -- At a new number
  | isDigit ch && lastIndexPreviousNumber /= i - 1 = acc ++ [(i, i)]
  -- Continuing a number
  | isDigit ch && lastIndexPreviousNumber == i - 1 = init acc ++ [(fst $ last acc, i)]
  -- Not at a number
  | otherwise = acc
  where
    lastIndexPreviousNumber = snd $ last acc

-- | Parses a string with numbers into a list of inclusive ranges for the numbers
--
-- # Examples
--
-- >>> parseLine ".12...14..%..4"
-- [(1,2),(6,7),(13,13)]
parseLine :: String -> [NumberRange]
parseLine = foldl numberParser [] . enumerate

-- | Obtains adjacent chars around a slice in a list of strings
--
-- # Examples
--
-- >>> getAdjacentChars [
--   "A-----B",
--   "@hello|",
--   "C+++++D"
-- ] (1, 1, 5)
-- "ABCD-----+++++|@"
--
-- >>> getAdjacentChars [
--   "A-----B",
--   "@hello|",
--   "C+++++D"
-- ] (0, 0, 3)
-- "l@hel-"
getAdjacentChars :: [String] -> LinenrNumberRange -> [Char]
getAdjacentChars linesList (linenr, from, to) =
  let lineAbove = linesList `maybeAt` (linenr - 1)
      line = linesList `maybeAt` linenr
      lineBelow = linesList `maybeAt` (linenr + 1)
      atMaybe = flip maybeAt
      prev = atMaybe $ from - 1
      next = atMaybe $ to + 1
      before = line >>= prev
      after = line >>= next
      topLeft = lineAbove >>= prev
      topRight = lineAbove >>= next
      bottomLeft = lineBelow >>= prev
      bottomRight = lineBelow >>= next
      leftRight = [before, after, topLeft, topRight, bottomLeft, bottomRight]
      topRow = slice from (to + 1) <$> lineAbove
      bottomRow = slice from (to + 1) <$> lineBelow
   in concat
        ( catMaybes
            [ topRow,
              bottomRow
            ]
        )
        ++ catMaybes leftRight

-- | A number is considered a part number if it's adjacent to a symbol
--
-- # Examples
--
-- >>> isPartNumber [
--   "...#...",
--   ".1234..",
--   "......."
-- ] (1, 1, 4)
-- True
--
-- >>> isPartNumber [
--   "......",
--   ".1234..",
--   "......."
-- ] (1, 1, 4)
-- False
isPartNumber :: [String] -> LinenrNumberRange -> Bool
isPartNumber linesList numberRange = any isSymbol $ getAdjacentChars linesList numberRange

-- | Adds line number information to each number position and flattens
--
-- # Examples
--
-- >>> addLineInfo [[(1, 1), (1, 1)], [(1, 1), (1, 1)]]
-- [(0, 1, 1), (0, 1, 1), (1, 1, 1), (1, 1, 1)]
addLineInfo :: [[NumberRange]] -> [LinenrNumberRange]
addLineInfo =
  concatMap
    ( \(line, numberPositions) ->
        map (\(pos1, pos2) -> (line, pos1, pos2)) numberPositions
    )
    . enumerate

parsePartNumbers :: [LinenrNumberRange] -> [String] -> [Int]
parsePartNumbers allNumbers allLines = map parsePartNumber allNumbers
  where
    parsePartNumber (linenr, from, to) = read (slice from (to + 1) (allLines !! linenr))

part1 :: String -> String
part1 input =
  let parsedNumbers = addLineInfo $ map parseLine allLines
      partNumbers = filter (isPartNumber allLines) parsedNumbers
      allLines = lines input
   in show $ sum $ parsePartNumbers partNumbers allLines
