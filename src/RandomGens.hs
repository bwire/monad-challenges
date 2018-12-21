module RandomGens where

import MCPrelude
import Data.Tuple

-- Random number generation
fiveReads :: [Integer]
fiveReads = fv' 5 (mkSeed 1)
  where 
    fv' :: Integer -> Seed -> [Integer]
    fv' 0 _ = []
    fv' n seed = let (ir, s') = rand seed
                 in ir : fv' (n - 1) s'

-- Random character generation
randLetter :: Seed -> (Char, Seed)
randLetter = swap . fmap toLetter . swap . rand

randString3 :: String
randString3 = rs' 3 (mkSeed 1)
  where
    rs' 0 _ = []
    rs' n seed = let (letter, s') = randLetter seed
                 in letter : rs' (n - 1) s'