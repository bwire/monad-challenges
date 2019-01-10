{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set5 where

import MCPrelude
import Set2 (Maybe (..), headMay, tailMay, lookupMay, maximumMay, divMay)
import Set3 (Card (..))

class Monad m where 
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  fail :: String -> m a
  fail = undefined

infixl 1 >>=  

sequence :: Monad m => [m a] -> m [a] 
sequence ms = foldr ff' (return []) ms
  where ff' m ma = do
        v <- m
        a <- ma
        return $ v:a

lift :: Monad m => (a -> b) -> m a -> m b
lift f ma = do
  a <- ma
  return $ f a 

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma 
  b <- mb 
  return $ f a b

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

join :: Monad m => m (m a) -> m a
join = (=<<) . (=<<) $ return 

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return $ f a b c 

ap :: Monad m => m (a -> b) -> m a -> m b    
ap mf mv = do
  f <- mf
  v <- mv
  return $ f v

-- generators
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  return = Gen . (,)
  (Gen fg) >>= k = Gen $ \s ->
    let (v, s') = fg s
    in runGen (k v) s'

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

makeRandom :: Gen Integer
makeRandom = Gen rand 

-- doesn't make any sense to use do-notation here
fiveRands :: Gen [Integer]
fiveRands = do
  let gens = replicate 5 makeRandom
  sequence gens

randLetter :: Gen Char
randLetter = do
  g <- Gen rand 
  return $ toLetter g

randString3 :: Gen String
randString3 = sequence $ replicate 3 randLetter

generalPair :: Gen a -> Gen b -> Gen(a, b)
generalPair = liftM2 (,)

-- Maybe
instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just v >>= k = k v

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData letter = do
  ints <- lookupMay letter gData 
  tail <- tailMay ints
  maxi <- maximumMay tail
  hd <- headMay ints
  divMay (fromIntegral maxi) (fromIntegral hd)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries list p1 p2 = do
  s1 <- lookupMay p1 list
  s2 <- lookupMay p2 list
  return $ s1 + s2

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  t <- tailMay xs
  return $ product t

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  t <- tailMay xs
  return $ sum t

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = do
  t <- tailMay xs
  return $ maximumMay t

-- list
instance Monad [] where
  return a = [a]
  (>>=) = flip concatMap

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

allCards :: [Int] -> [String] -> [Card]
allCards xis xss = do
  i <- xis
  s <- xss
  return $ Card i s

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
  x <- xs
  y <- ys
  z <- zs
  return $ f x y z