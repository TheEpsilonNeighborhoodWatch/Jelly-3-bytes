import Control.Monad
import Functoid

data ProgTree a = ProgTree (Functoid a) [ProgTree a]

flatten :: ProgTree a -> Functoid a
flatten (ProgTree f []) = f
flatten (ProgTree f ls) = foldl (join composition) f $ map flatten ls

stitch :: [Functoid a] -> [ProgTree a] -> [ProgTree a]
stitch [] [] = []
stitch (f@(Functoid _ _ ar):xs) ys = ProgTree f (take ar ys) : stitch xs (drop ar ys)

buildTree :: [[Functoid a]] -> ProgTree a
buildTree = head . (foldr stitch []) 

stratify :: Int -> [Functoid a] -> [[Functoid a]]
stratify x [] = []
stratify x fs = layer : stratify (sum $ map arity layer) (drop x fs) 
 where layer = take x fs

