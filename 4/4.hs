import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main = do  
    content <- readFile "4.txt"
    let firstPart  = sum . map (\x->containsAssignment . map (\y->map read $ splitOn "-" y) $ splitOn "," x) $ lines content
    let secondPart = sum . map (\x->overlaps . map (\y->map read $ splitOn "-" y) $ splitOn "," x) $ lines content

    print firstPart
    print secondPart

containsAssignment :: [[Integer]] -> Integer
containsAssignment ((a:b:[]):(x:y:[]):_)
    | (a <= x && b >= y) || (x <= a && y >= b) = 1
    | otherwise                                = 0
containsAssignment _ = error "asd"

overlaps :: [[Integer]] -> Integer
overlaps ((a:b:[]):(x:y:[]):_)
    | not . null $ intersect [a..b] [x..y] = 1
    | otherwise                            = 0
overlaps _ = error "asd"