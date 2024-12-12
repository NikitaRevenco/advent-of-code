module Day3 where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)
-- import Debug.Trace (traceShowId)
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

-- | (line number, index)
type CharPosition = (Int, Int)

-- | Obtains adjacent chars around a slice in a list of strings
--
-- # Examples
--
-- >>> getAdjacentChars [ "A-----B", "@hello|", "C+++++D" ] $ getAdjacentCharPositions [ "A-----B", "@hello|", "C+++++D" ] (1, 1, 5)
--   "A-----B",
--   "@hello|",
--   "C+++++D"
-- ] (1, 1, 5)
-- "ABCD-----+++++|@"
--
-- >>> getAdjacentChars [ "A-----B", "@hello|", "C+++++D" ] $ getAdjacentCharPositions [ "A-----B", "@hello|", "C+++++D" ] (0, 1, 5)
-- "@hello|AB"
getAdjacentChars :: [String] -> [CharPosition] -> [Char]
getAdjacentChars linesList = map (\(charLinenr, charIndex) -> (linesList !! charLinenr) !! charIndex)

-- | Obtains positions of chars around a slice in a list of strings
--
-- # Examples
--
-- >>> getAdjacentChars [
--   "A-----B",
--   "@hello|",
--   "C+++++D"
-- ] (1, 1, 5)
-- [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6),
--  (1, 0),                                         (1, 6),
--  (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6)]
getAdjacentCharPositions :: [String] -> LinenrNumberRange -> [CharPosition]
getAdjacentCharPositions linesList (linenr, from, to) =
  let linenrAbove = linenr - 1
      lineAbove = linesList `maybeAt` linenrAbove
      line = linesList `maybeAt` linenr
      linenrBelow = linenr + 1
      lineBelow = linesList `maybeAt` linenrBelow

      checkIndex charIndex lineNumber someLine
        | charIndex < 0 || charIndex >= length someLine = Nothing
        | otherwise = Just (lineNumber, charIndex)

      before = line >>= checkIndex (from - 1) linenr
      after = line >>= checkIndex (to + 1) linenr
      topLeft = lineAbove >>= checkIndex (from - 1) linenrAbove
      topRight = lineAbove >>= checkIndex (to + 1) linenrAbove
      bottomLeft = lineBelow >>= checkIndex (from - 1) linenrBelow
      bottomRight = lineBelow >>= checkIndex (to + 1) linenrBelow

      -- leftRight = []
      leftRight = [before, after, topLeft, topRight, bottomLeft, bottomRight]

      topSliced = (slice from (to + 1) <$> lineAbove)
      bottomSliced = (slice from (to + 1) <$> lineBelow)
      topZipped = flip zip [from ..] <$> topSliced
      bottomZipped = flip zip [from ..] <$> bottomSliced

      topRow = (((linenrAbove,) . snd) <$>) <$> topZipped
      bottomRow = (((linenrBelow,) . snd) <$>) <$> bottomZipped
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
isPartNumber linesList numberRange = any isSymbol $ getAdjacentChars linesList $ getAdjacentCharPositions linesList numberRange

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

getAllPartNumbers :: [String] -> [LinenrNumberRange]
getAllPartNumbers allLines = filter (isPartNumber allLines) $ addLineInfo $ map parseLine allLines

part1 :: String -> String
part1 input = show $ sum $ parsePartNumbers (getAllPartNumbers $ lines input) $ lines input
