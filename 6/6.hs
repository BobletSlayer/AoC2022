import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main = do  
    content <- readFile "6.txt"

    let firstPart = nUnique 4 content
    let secondPart =  nUnique 14 content

    print firstPart
    print secondPart

nUnique :: Integer -> String -> Integer
nUnique n xs = unique n xs where
    unique x [] = x
    unique x l@(y:ys) 
        | (genericLength . nub $ genericTake n l) == n = x
        | otherwise                            = unique (x+1) ys