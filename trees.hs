import Control.Monad

data ProgTree a = ProgTree (Functoid a) [ProgTree a]

data Functoid a = Functoid {
 composition :: Functoid a -> Functoid a -> Functoid a,
 application :: [a] -> a,
 arity :: Int
}

flatten :: ProgTree a -> Functoid a
flatten (ProgTree f []) = f
flatten (ProgTree f ls) = foldl (join composition) f $ map flatten ls
