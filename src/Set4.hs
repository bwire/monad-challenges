{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where

import MCPrelude
import Set2

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = bind 

infixl 1 >>=  

sequence :: Monad m => [m a] -> m [a] 
sequence ms = foldr (\m ma -> bind m (\v -> bind ma (\a -> return $ v:a))) (return []) ms

lift :: Monad m => (a -> b) -> m a -> m b
lift f ma = ma >>= return . f 

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\a -> bind mb (\b -> return $ f a b))

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join = flip bind . flip bind $ return 

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma $
  \a -> bind mb $
  \b -> bind mc $
  \c -> return $ f a b c 

ap :: Monad m => m (a -> b) -> m a -> m b    
ap mf mv =  bind mf $
  \f -> bind mv $
  \v -> return $ f v

-- Generators overwritten using monad
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  return = Gen . (,)
  bind (Gen fg) k = Gen $ \s ->
    let (v, s') = fg s
    in runGen (k v) s'

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

genInt :: Gen Integer
genInt = Gen rand 

-- Random number generation
fiveRands :: [Integer]
fiveRands = evalGen (sequence gens) $ mkSeed 1
  where gens = replicate 5 genInt

-- Random character generation
randLetter :: Gen Char
randLetter = Gen rand >>= return . toLetter

randString3 :: String
randString3 = evalGen (sequence gens) $ mkSeed 1
  where gens = replicate 3 randLetter

generalA :: (a -> b) -> Gen a -> Gen b
generalA = lift

randEven :: Gen Integer
randEven = lift (*2) genInt

randOdd :: Gen Integer
randOdd = lift (+1) randEven

randTen :: Gen Integer
randTen = lift (*10) genInt

-- Generalizing Random Pairs
randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter genInt

generalPair :: Gen a -> Gen b -> Gen(a, b)
generalPair = liftM2 (,)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

repRandom :: [Gen a] -> Gen [a] 
repRandom = sequence

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

mkGen :: a -> Gen a
mkGen = return 

instance Monad Maybe where
  return = mkMaybe
  bind = flip chain

instance Monad [] where
  return a = [a]
  bind = flip concatMap