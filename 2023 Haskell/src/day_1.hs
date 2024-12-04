module Day (add) where

data Person = Person
  { firstName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

add :: (Num a) => a -> a -> a
add = (+)
