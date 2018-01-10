
data ProgTree a = ProgTree (Functoid a) [ProgTree a]

data Functoid a = Functoid {
 composition :: Functoid a -> Functoid a,
 application :: [a] -> a,
 arity :: Int
}

compose :: Functoid a -> Functoid a -> Functoid a
compose (Functoid comp _ _) f = comp f

flatten :: ProgTree a -> Functoid a
flatten (ProgTree f []) = f
flatten (ProgTree f ls) = foldl compose f $ map flatten ls
