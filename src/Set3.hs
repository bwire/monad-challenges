{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xas = concat . foldr (\b acc -> map (flip (,) b) xas : acc) []  
-- I could have done this using Applicative
-- allPairs xas xbs = (,) <$> xas <*> xbs 

-- Poker hands
data Card = Card (Int, String)

instance Show Card where
  show (Card(i, s)) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards is = concat . foldr (\b acc -> map (Card . flip (,) b) is : acc) []

-- Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f xas xbs = concat . foldr (\a acc -> map (f a) xbs : acc) [] $ xas

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = (map Card .) . allPairs

