module Day3 where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow, traceShowId)
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

sumPartNumbers :: [LinenrNumberRange] -> [String] -> Int
sumPartNumbers linenrrange strList = sum $ map f linenrrange
  where
    f (linenr, from, to) = read (slice from (to + 1) (strList !! linenr))

rolf = ".........................3.......................................94...............806....................596.........793...........186......\n.../..........*574.587..*........161......904.......412.........*.................*.................................=.....637.%......*......"

-- haha = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..\n"

ahha = isPartNumber (lines rolf)

linez = lines rolf

strz = [".........................3.......................................94...............806....................596.........793...........186......", ".../..........*574.587..*........161......904.......412.........*.................*.................................=.....637.%......*......"]

three :: (Int, Int, Int)
three = (0, 25, 25)

{-
BUG: isPartNumber linez (0,82,84) => True (which corresponds to the "806")

BUG: getAdjacentChars linez (0, 82, 84) => "*.." but should be "*......"

BUG: getAdjacentChars strz three
"."
should be
"....*"

-}

part1 :: String -> String
part1 lol = show $ sumPartNumbers partNumbers linez
  where
    numberPositions = addLineInfo $ map parseLine linez
    partNumbers = filter (isPartNumber linez) numberPositions
    linez = lines lol

-- data NumberSlice = NumberSlice {line :: Int, xrange :: (Int, Int)}

-- lol :: String -> [(Int, Char)]
-- lol = zip [0 ..]

-- lineNumberCoords :: [(Char, Int)] -> [(Int, Int)]
-- lineNumberCoords (x@(ch, num) : xs) = x : lineNumberCoords xs

-- -- getAdjacent :: [String] -> (Int, Int) -> [Char]

-- -- getAdjacent

-- -- isSymbolAdjacent :: String -> Int -> Bool
-- -- isSymbolAdjacent =

-- part1 :: String -> String
-- part1 = id

-- part2 :: String -> String
-- part2 = id
