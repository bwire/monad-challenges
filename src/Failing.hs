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
      mmax = chain maximumMay . chain tailMay $ mxs
      mhead = chain headMay $ mxs
  in chain (\a -> chain (\b -> divMay (fromIntegral a) (fromIntegral b)) mhead) mmax  
 
-- Chaining variations
mkMaybe :: a -> Maybe a
mkMaybe = Just

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries list p1 p2 = 
  chain (\s1 -> chain ((\s2 -> mkMaybe (s1 + s2))) (lookupMay p2 list)) (lookupMay p1 list)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = chain (\a -> chain (\b -> mkMaybe $ f a b) mb) ma

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 list p1 p2 = yLink (+) (lookupMay p1 list) (lookupMay p2 list)

-- Tailprod
tailProd :: Num a => [a] -> Maybe a
tailProd xs = 
  case tailMay xs of
    Nothing -> Nothing
    Just t -> mkMaybe . product $ t

tailSum :: Num a => [a] -> Maybe a
tailSum xs = 
  case tailMay xs of
    Nothing -> Nothing
    Just t -> mkMaybe . sum $ t

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f = chain (mkMaybe . f)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = transMaybe product . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = transMaybe sum . tailMay

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine = chain . chain $ mkMaybe  

tailMax2 :: Ord a => [a] -> Maybe a
tailMax2 = combine . tailMax

tailMin2 :: Ord a => [a] -> Maybe a
tailMin2 = combine . tailMin
  
