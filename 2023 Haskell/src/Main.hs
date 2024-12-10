module Main where

import qualified Day1

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
