{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xas = concat . foldr (\b acc -> map (flip (,) b) xas : acc) []  
-- I could have done this using Applicative
-- allPairs xas xbs = (,) <$> xas <*> xbs 

-- Poker hands
data Card = Card Int String

instance Show Card where
  show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards is = concat . foldr (\b acc -> map (flip Card b) is : acc) []

-- Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f xas xbs = concatMap (flip map xbs . f) xas

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card

-- Combinations of three things
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xas xbs xcs = concatMap (\a -> allCombs (f a) xbs xcs) xas
 
-- Combinations of more things
combStep :: [a -> b] -> [a] -> [b]    
combStep = zipWith ($)

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = map f xs `combStep` ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `combStep` ys `combStep` zs  

allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4' f as bs cs ds = map f as `combStep` bs `combStep` cs `combStep` ds



