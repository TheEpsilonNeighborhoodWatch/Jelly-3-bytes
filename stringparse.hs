module StringParse (semiparse,visualparse) where

import Functoid

-- Temporary
type Object = Integer

tokenparse :: String -> Functoid Object
tokenparse "+" = (Functoid standardCompose (\[a,b] -> a + b) 2)
tokenparse "*" = (Functoid standardCompose (\[a,b] -> a * b) 2)
tokenparse "-" = (Functoid standardCompose (\[a,b] -> a - b) 2)
tokenparse "^" = (Functoid standardCompose (\[a,b] -> a ^ b) 2)
tokenparse "/" = (Functoid standardCompose (\[a,b] -> div a b) 2)
tokenparse "%" = (Functoid standardCompose (\[a,b] -> mod a b) 2)
tokenparse "0" = (Functoid standardCompose (\[] -> 0) 0)
tokenparse "1" = (Functoid standardCompose (\[] -> 1) 0)
tokenparse "2" = (Functoid standardCompose (\[] -> 2) 0)
tokenparse "3" = (Functoid standardCompose (\[] -> 3) 0)
tokenparse "4" = (Functoid standardCompose (\[] -> 4) 0)
tokenparse "5" = (Functoid standardCompose (\[] -> 5) 0)
tokenparse "6" = (Functoid standardCompose (\[] -> 6) 0)
tokenparse "7" = (Functoid standardCompose (\[] -> 7) 0)
tokenparse "8" = (Functoid standardCompose (\[] -> 8) 0)
tokenparse "9" = (Functoid standardCompose (\[] -> 9) 0)

-- Parser for the interpreter --
semiparse :: String -> [Functoid Object]
semiparse [] = []
semiparse (a:x) = tokenparse (a:[]) : semiparse x

-- Parser for the visualizer --
visualparse :: String -> [(String,Int)]
visualparse [] = []
visualparse (a:x) = (a:[],arity $ tokenparse $ a:[]) : visualparse x
