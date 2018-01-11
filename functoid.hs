module Functoid (standardCompose, Functoid(..)) where

standardCompose :: Functoid a -> Functoid a -> Functoid a
standardCompose (Functoid comp1 app1 ar1) (Functoid _ app2 ar2) = Functoid standardCompose newApp (ar1 + ar2 -1)
 where newApp l
        | length l == ar1 + ar2 - 1 = app1 $ app2 (drop (ar1 - 1) l) : take (ar1 - 1) l

data Functoid a = Functoid {
 composition :: Functoid a -> Functoid a -> Functoid a,
 application :: [a] -> a,
 arity :: Int
}
