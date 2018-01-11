module StringParse (semiparse,visualparse) where

import Functoid

-- Temporary
type Object = Integer

tokenparse :: String -> Functoid Object
tokenparse "+" = (Functoid standardCompose (\[a,b] -> a + b) 2)
tokenparse "*" = (Functoid standardCompose (\[a,b] -> a * b) 2)
tokenparse "-" = (Functoid standardCompose (\[a,b] -> a - b) 2)
tokenparse "0" = (Functoid standardCompose (\[] -> 0) 0)
tokenparse "1" = (Functoid standardCompose (\[] -> 1) 0)

-- Parser for the interpreter --
semiparse :: String -> [Functoid Object]
semiparse [] = []
semiparse (a:x) = tokenparse (a:[]) : semiparse x

-- Parser for the visualizer --
visualparse :: String -> [(String,Int)]
visualparse [] = []
visualparse (a:x) = (a:[],arity $ tokenparse $ a:[]) : visualparse x
