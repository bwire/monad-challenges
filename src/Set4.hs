{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where

import MCPrelude
import Set2

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b

sequence :: Monad m => [m a] -> m [a] 
sequence ms = foldr (\m ma -> bind m (\v -> bind ma (\a -> return $ v:a))) (return []) ms

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

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  return = Gen . (,)
  bind (Gen fg) k = Gen $ \s ->
    let (v, s') = fg s
    in runGen (k v) s'

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

-- Random number generation
fiveReads :: [Integer]
fiveReads = fst . runGen (sequence gens) $ mkSeed 1
  where gens = replicate 5 $ Gen rand 

instance Monad Maybe where
  return = mkMaybe
  bind = flip chain

instance Monad [] where
  return a = [a]
  bind = flip concatMap