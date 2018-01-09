type Object = Int

data Functoid a b = Functoid {
 implementation :: ([a] -> b),
 arity :: Int
}

instance Functor (Functoid x) where
--fmap :: (a -> b) -> Functoid x a -> Functoid x b
  fmap f (Functoid impl ar) = Functoid (f . impl) ar
--(<$) = fmap . const 

instance Applicative (Functoid a) where
 pure x = Functoid (\[] -> x) 0
 
stitch :: [Functoid a a] -> [Functoid a a] -> [Functoid a a]
stitch [] [] = []
stitch (x@(Functoid _ 0):xs) ys = x : stitch xs ys
stitch ((Functoid impl ar):xs) ys = pure (impl $ map (($[]).implementation) $ take ar ys) : stitch xs (drop ar ys)

evaluate :: [[Functoid a a]] -> a
evaluate (x:y:rest) = evaluate $ stitch y x : rest
evaluate [[Functoid impl 0]] = impl [] 
