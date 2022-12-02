import System.IO  
import Control.Monad
import Data.List
import Data.List.Split

main = do  
    content <- readFile "2.txt"
    let firstPart = sum . map (\(a:_:c:_) -> calcScore a c) $ splitOn "\n" content
    let secondPart = sum .  map (\(a:_:c:_) -> calcScore a (f a c)) $ splitOn "\n" content

    print firstPart
    print secondPart

calcScore :: Char -> Char -> Integer
calcScore x y = (outcome x y) + (shape y) 

outcome :: Char -> Char -> Integer
outcome x y 
    | (x == 'A' && y == 'Z') || (x == 'B' && y == 'X') || (x == 'C' && y == 'Y')  = 0
    | (x == 'A' && y == 'Y') || (x == 'B' && y == 'Z') || (x == 'C' && y == 'X')  = 6
    | otherwise                                                                   = 3

shape :: Char -> Integer
shape 'X' = 1
shape 'Y' = 2
shape 'Z' = 3
shape _   = 0

f :: Char -> Char -> Char
f x 'X' = case x of 'A' -> 'Z'; 'B' -> 'X'; 'C' -> 'Y'
f x 'Y' = case x of 'A' -> 'X'; 'B' -> 'Y'; 'C' -> 'Z'
f x 'Z' = case x of 'A' -> 'Y'; 'B' -> 'Z'; 'C' -> 'X'