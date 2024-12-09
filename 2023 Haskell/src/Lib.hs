module Lib where

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Cannot extract value from Nothing"
