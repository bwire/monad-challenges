{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where

import MCPrelude
import Set2 (Maybe (..))
import Set3 (Card (..))

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

-- Maybe overwritten using monad
instance Monad Maybe where
  return = Just
  bind Nothing _ = Nothing
  bind (Just v) k = k v

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = return x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = return xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay v xs = foldl ff Nothing xs
  where ff acc (a, b) = case acc of 
                          Nothing -> if v == a then return b else Nothing 
                          Just a -> acc

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay n d | d == 0 = Nothing
           | otherwise = return $ n / d

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = return $ foldr max x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = return $ foldr min x xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData letter = 
  lookupMay letter gData >>= 
    \xs -> tailMay xs >>=
    maximumMay >>=
    \mxs -> headMay xs >>=
    \h -> divMay (fromIntegral mxs) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain = flip bind

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = bind

mkMaybe :: a -> Maybe a
mkMaybe = return

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries list p1 p2 = liftM2 (+) (lookupMay p1 list) (lookupMay p2 list)
 
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink = liftM2

-- Tailprod
tailProd :: Num a => [a] -> Maybe a
tailProd = lift product . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = lift sum . tailMay

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe = lift  

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = lift maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = lift minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine = join  

tailMax2 :: Ord a => [a] -> Maybe a
tailMax2 = join . tailMax

tailMin2 :: Ord a => [a] -> Maybe a
tailMin2 = join . tailMin


-- List overwritten using monad
instance Monad [] where
  return a = [a]
  bind = flip concatMap

combStep :: [a -> b] -> [a] -> [b]    
combStep = ap

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f xas xbs = xas >>= \a -> xbs >>= \b -> [f a b]

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,) 

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card 

-- Generalizing pairs and cards
allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = map f xs `ap` ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `ap` ys `ap` zs  

allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4' f as bs cs ds = map f as `ap` bs `ap` cs `ap` ds



