import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main = do  
    content <- readFile "5.txt"
    let crates = reverse . map (f 0 ) $ init $ lines $ ((splitOn "\n\n" content) !! 0)
    let moves  = reverse . map (\xs-> (\(_:a:_:b:_:c:_)-> ((read a):((read b) - 1):((read c) - 1):[])) $ words xs) $ lines $ (splitOn "\n\n" content) !! 1
    let count  = maximum . map (read) $ words . last . lines $ (splitOn "\n\n" content) !! 0
    let filled = fillCols (take count $ repeat "") crates

    let firstPart = map head $ foldr (\x y -> move x y) filled moves
    let secondPart = map head $ foldr (\x y -> moveS x y) filled moves

    print firstPart
    print secondPart

f :: Integer -> String -> [(Integer,Char)]
f y []                   = []
f y (' ':' ':' ':' ':xs) = f (y + 1) xs
f y (' ':xs)             = f y xs
f y ('[':x:']':xs)       = (y,x) : f (y + 1) xs

fillCols :: [String] -> [[(Integer, Char)]] -> [String]
fillCols cols []        = cols
fillCols cols (x:xs)    = fillCols (fillCols' cols x) xs where 
    fillCols' cols []         = cols
    fillCols' cols ((x,y):xs) = fillCols' (genericTake x (cols) ++ [y:(cols !! (fromIntegral x))] ++ genericDrop (x + 1) cols) xs

move :: [Integer] -> [String] -> [String]
move (0:from:to:[]) cols     = cols 
move (count:from:to:[]) cols = move ((count-1):from:to:[]) (remove (addToCol cols (head $ cols !! (fromIntegral from)) to) from) where
    addToCol cols y x = (genericTake x (cols) ++ [y:(cols !! (fromIntegral x))]     ++ genericDrop (x + 1) cols)
    remove cols x     = (genericTake x (cols) ++ [tail $ cols !! (fromIntegral x)]  ++ genericDrop (x + 1) cols)

moveS :: [Integer] -> [String] -> [String]
moveS (count:from:to:[]) cols = removeS count from (addS count from to cols) where
    addS count from to cols = genericTake to cols ++ [(genericTake count (cols !! (fromIntegral from)))++(cols !! (fromIntegral to))] ++ genericDrop (to + 1) cols
    removeS count from cols = genericTake from cols ++ [genericDrop count (cols !! (fromIntegral from))] ++ genericDrop (from + 1) cols