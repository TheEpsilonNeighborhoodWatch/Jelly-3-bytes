{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

regPoly' :: (V t ~ V2, TrailLike t) => Int -> N t -> t
regPoly' faces radius = polygon $ PolygonOpts (PolyRegular faces radius) NoOrient (P (V2 0 0))

node :: String -> Int -> Diagram B
node n i = text n # fontSizeL 0.2 # fc black <> circle 0.125 #lw none # named i

ring :: Double -> [(String,Int)] -> Diagram B 
ring size l = atPoints (trailVertices $ regPoly' (length l) size) (map (uncurry node) l) `atop` circle (size + 0.5) # lc gray

form :: [[(String,Int)]] -> Diagram B
form = (foldl1 atop).(zipWith ring [0..])

stratify :: Int -> [(a,Int)] -> [[a]]
stratify _ [] = []
stratify x ys = map fst layer : stratify (sum $ map snd layer) (drop x ys)
 where layer = take x ys

connections :: Int -> Int -> [(a,Int)] -> [(Int,Int)]
connections _ _ [] = []
connections offset readhead ((_,ar):xs) = [(offset,readhead+x)|x<-[1..ar]] ++ connections (offset + 1) (readhead + ar) xs 

structure = [("ø",2),("å",3),("∑",2),("∫",1),("≤",2),("¬",1),("@",0),("∂",2),("&",0),("1",0),("∆",0),("@",0),("¥",0),("æ",0)]

example = foldr (uncurry connectOutside) (form $ stratify 1 $ zipWith (\x (a,b)->((a,x),b)) [0..] structure) $ connections 0 0 structure 
--example =(form $ stratify 1 $ zipWith (\x (a,b)->((a,x),b)) [0..] structure)

main = mainWith example
