module Failing where

import Prelude (undefined) 
import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

-- library
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay v ((a, b):ts) | v == a = Just b
                        | otherwise = lookupMay v ts 

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay n d | d == 0 = Nothing
           | otherwise = Just $ n / d

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldr max x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just $ foldr min x xs