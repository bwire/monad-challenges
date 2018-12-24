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


-- chains of failing computations
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData letter =  
  case lookupMay letter gData of
    Nothing -> Nothing
    Just xs ->
      case tailMay xs of 
        Nothing -> Nothing
        Just txs ->
          case maximumMay txs of 
            Nothing -> Nothing
            Just mxs -> 
              case headMay xs of
                Nothing -> Nothing
                Just h -> divMay (fromIntegral mxs) (fromIntegral h)

-- Generalizing chains of failures
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain k (Just v) = k v

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gData letter = 
  let mxs = lookupMay letter gData
      mmax = chain (Just . fromIntegral) . chain maximumMay . chain tailMay $ mxs
      mhead = chain (Just . fromIntegral) . chain headMay $ mxs
  in chain (\a -> chain (\b -> divMay a b) mhead) mmax  

  
