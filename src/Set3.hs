{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xas = concat . foldr (\b acc -> map (flip (,) b) xas : acc) []  
-- I could have done this using Applicative
-- allPairs xas xbs = (,) <$> xas <*> xbs 