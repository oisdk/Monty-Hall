import Control.Applicative
import Data.Ratio
import Data.List
import Data.Ord
import Data.Maybe
import System.Random
import Data.Tuple

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
  fmap f (Prob xs) = Prob $ fmapFst f <$> xs
    
instance Applicative Prob where
  pure a = Prob [(a,1)]
  (Prob ys) <*> (Prob xs) = Prob [(f x, px*py)|(f,py) <- ys, (x,px) <- xs]
  
instance Monad Prob where
  return = pure
  (Prob xs) >>= f = Prob [(y,px*py)|(x,px) <- xs, (y,py) <- getProb(f x)]
  
equalProbs :: [a] -> Prob a
equalProbs x = Prob $ map (flip (,) (1%n))  x
  where n = fromIntegral (length x)
     
fmapFst :: (a -> b) -> (a,c) -> (b,c)
fmapFst f (a,b) = (f a, b)

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)
   
mergeProbs :: Ord a => Prob a -> Prob a
mergeProbs (Prob xs) = Prob $ mergeBy (comparing fst) (fmap . (+) . snd) xs

isEq :: Ordering -> Bool
isEq EQ = True
isEq _  = False

mergeBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a]
mergeBy c m xs = mergeBySt eq m (sortBy c xs)
  where eq x y = isEq $ c x y

mergeBySt :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a]
mergeBySt e c = unfoldr $ fmap sumUp . uncons
  where sumUp (l,ls) = fmapFst (foldl' c l) (break (not . e l) ls)

data Choice = Switch | Stick

chances :: Int -> Int -> Choice -> Int -> Prob Bool
chances n _ Stick  d = fmap (==d) (equalProbs [1..n])
chances n p Switch d = (&&) . not          <$>
                       chances n p Stick d <*>
                       (equalProbs $ True : replicate (n-p-2) False)

chanceOfCar :: Int -> Int -> Choice -> Prob Bool
chanceOfCar n p s = mergeProbs $
                    equalProbs [1..n] >>= 
                    chances n p s

choose :: (Prob a) -> (IO a)
choose (Prob x) = (fromN x) <$> fromIntegral <$> (getStdRandom (randomR (1,1000000)))
  where fromN ((x,_):[]) _ = x
        fromN ((x,p):xs) n
              | v < 0     = x
              | otherwise = fromN xs v
              where v = n - p * 1000000
              

                                       
