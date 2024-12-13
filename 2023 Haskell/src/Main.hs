module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day7

assertEq :: IO String -> String -> IO ()
assertEq actual_io expected = do
  actual <- actual_io
  if actual == expected
    then return ()
    else error $ "Assertion failed for " ++ actual ++ " == " ++ expected

readInput :: Int -> IO String
readInput day = do
  readFile $ "inputs/day_" ++ show day ++ ".txt"

main :: IO ()
main = do
  assertEq (Day1.part1 <$> readInput 1) "54388"
  assertEq (Day1.part2 <$> readInput 1) "53515"
  assertEq (Day2.part1 <$> readInput 2) "2679"
  assertEq (Day2.part2 <$> readInput 2) "77607"
  assertEq (Day3.part1 <$> readInput 3) "550064"
  assertEq (Day4.part1 <$> readInput 4) "21919"
  assertEq (Day7.part1 <$> readInput 7) "1"

-- a <- readInput 2
-- putStr $ Day2.part2 a
