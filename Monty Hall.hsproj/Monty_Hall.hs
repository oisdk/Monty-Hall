import Control.Applicative
import Data.Ratio
import Data.List
import Data.Maybe
import Data.Ord

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
  fmap f (Prob xs) = Prob [(f x,p)|(x,p) <- xs]
    
instance Applicative Prob where
  pure a = Prob [(a,1)]
  (Prob ys) <*> (Prob xs) = Prob [(f x, px*py)|(f,py) <- ys, (x,px) <- xs]
  
instance Monad Prob where
  return = pure
  (Prob xs) >>= f = Prob [(y,px*py)|(x,px) <- xs, (y,py) <- getProb $ f x]
  
equalProbs :: [a] -> Prob a
equalProbs x = Prob $ map withProb x
  where withProb a = (a,1%n)
        n = fromIntegral $ length x
        
mergeProbs :: Ord a => Prob a -> Prob a
mergeProbs (Prob xs) = Prob $ mergeBy (comparing fst) addProb xs
  where addProb (a,pa) (_,pb) = (a,pa+pb)
          
mergeBy :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a]
mergeBy _ _ [] = []
mergeBy c m xs = mergeWith h [] t
  where (h,t)  = (head sorted, tail sorted)
        sorted = sortBy c xs
        mergeWith h p [] = h:p
        mergeWith h p (y:ys) = case c h y of
          EQ        -> mergeWith (m h y) p ys
          otherwise -> mergeWith y (h:p) ys

data Choice = Switch | Stick

chances :: (Int,Choice) -> Prob Bool
chances (d,Stick ) = fmap (==d) (equalProbs [1..2])
chances (d,Switch) = fmap (/=d) (equalProbs [1..2])

chanceOfCar :: Choice -> Prob Bool
chanceOfCar s = mergeProbs $ equalProbs (map (flip (,) s) [1..2]) >>= chances