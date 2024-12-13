module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map

-- | Counts how many times each item occurs in the list
countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = foldl countOccurrence Map.empty
  where
    countOccurrence map item = Map.insertWith (+) item 1 map

maybeAt :: [a] -> Int -> Maybe a
maybeAt [] _ = Nothing
maybeAt (x : xs) 0 = Just x
maybeAt (x : xs) index = maybeAt xs (index - 1)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Cannot extract value from Nothing"

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing other = other

{- Extracts a part of a list from index `from` up to, but not including index `to` -}
slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from) . drop from

splitByCh :: Char -> String -> [String]
splitByCh ch string = case dropWhile (== ch) string of
  "" -> []
  string' -> beforeChar : splitByCh ch restOfString
    where
      (beforeChar, restOfString) = break (== ch) string'

{- Removes leading and trailing whitespace -}
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
