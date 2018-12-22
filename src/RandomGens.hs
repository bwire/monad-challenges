module RandomGens where

import MCPrelude
import Control.Applicative

type Gen t = Seed -> (t, Seed)

-- Random number generation
fiveReads :: [Integer]
fiveReads = fv' 5 (mkSeed 1)
  where 
    fv' :: Integer -> Seed -> [Integer]
    fv' 0 _ = []
    fv' n seed = let (ir, s') = rand seed
                 in ir : fv' (n - 1) s'

-- Random character generation
randLetter :: Gen Char
randLetter = (\(i, s) -> (toLetter i, s)) . rand

randString3 :: String
randString3 = rs' 3 (mkSeed 1)
  where
    rs' 0 _ = []
    rs' n seed = let (letter, s') = randLetter seed
                 in letter : rs' (n - 1) s'

-- More Generators
generalA :: (a -> b) -> Gen a -> Gen b
generalA f rnd = (\(t, s) -> (f t, s)) . rnd

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

-- Generalizing Random Pairs
randPair :: Gen (Char, Integer)
randPair seed = 
  let (i, ns) = rand seed
      (i', ns') = rand ns
  in ((toLetter i, i'), ns')

generalPair :: Gen a -> Gen b -> Gen(a, b)
generalPair ga gb = \s0 ->
  let (v1, s1) = ga s0
      (v2, s2) = gb s1
  in ((v1, v2), s2)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb = \s0 ->
  let (v1, s1) = ga s0
      (v2, s2) = gb s1
  in (f v1 v2, s2)

randPair' :: Gen (Char, Integer)
randPair' = generalB (,) randLetter rand  
  