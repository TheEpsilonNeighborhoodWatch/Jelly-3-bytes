type Object = Int

data ArgumentGroup a =
 None |
 Single a |
 Double a a |
 Triplet a a a
 deriving Show

data Functoid a b = Functoid {
 implementation :: (ArgumentGroup a -> b),
 arity :: Int
}

instance Functor (Functoid x) where
--fmap :: (a -> b) -> Functoid x a -> Functoid x b
  fmap f (Functoid impl ar) = Functoid (f . impl) ar
--(<$) = fmap . const 

instance Applicative (Functoid a) where
 pure x = Functoid (\None -> x) 0
 

wrap :: (ArgumentGroup a -> b) -> (ArgumentGroup a -> Functoid a b)
wrap impl argG = Functoid (\None -> impl $ argG ) 0

stitch :: [Functoid a a] -> [Functoid a a] -> [Functoid a a]
stitch [] [] = []
stitch (x@(Functoid _ 0):xs) ys = x : stitch xs ys
stitch ((Functoid impl 1):xs) ((Functoid y 0):ys) = wrap impl (Single $ y None) : stitch xs ys
stitch ((Functoid impl 2):xs) ((Functoid y1 0):(Functoid y2 0):ys) = wrap impl (Double (y1 None) (y2 None)) : stitch xs ys
stitch ((Functoid impl 3):xs) ((Functoid y1 0):(Functoid y2 0):(Functoid y3 0):ys) = wrap impl (Triplet (y1 None) (y2 None) (y3 None)) : stitch xs ys

evaluate :: [[Functoid a a]] -> a
evaluate (x:y:rest) = evaluate $ stitch y x : rest
evaluate [[Functoid impl 0]] = impl None 
