{-# LANGUAGE ViewPatterns #-}

import System.IO  
import Control.Monad
import Data.List
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
-- import Data.FileTree

main = do  
    content <- readFile "8.txt"

    let colLength = genericLength $ lines content
    let matrix = map (foldr (\x y-> ((read [x])::Integer):y) []) $ lines content
    let tMatrix = transpose matrix
    let positions =  [(x,y) | x <- [1..(colLength)], y <- [1..(((\(x:xs) -> genericLength x) matrix))]]
    let posVisible =  (map (visible matrix tMatrix) positions)
    let firstpart = sum . map (\x-> if x then 1 else 0) $ posVisible
    let secondPart = maximum (map (scenicScore matrix tMatrix) positions)

    -- print (zip positions posVisible)
    print firstpart
    print secondPart

visible :: (Ord a, Integral b) => [[a]] -> [[a]] -> (b,b) -> Bool
visible xs ys (x,y) = (left || right || top || bot) where
    left  = vis $ genericTake x (xs !! ((fromIntegral y) - 1))
    right = vis $ reverse $ genericDrop (x - 1) (xs !! ((fromIntegral y) - 1))
    top  = vis $ genericTake y (ys !! ((fromIntegral x) - 1))
    bot = vis $ reverse $ genericDrop (y - 1) (ys !! ((fromIntegral x) - 1))

scenicScore :: (Ord a, Integral b) => [[a]] -> [[a]] -> (b,b) -> b
scenicScore xs ys (x,y) = (left * right * top * bot) where
    left  = countScore $ genericTake x (xs !! ((fromIntegral y) - 1))
    right = countScore $ reverse $ genericDrop (x - 1) (xs !! ((fromIntegral y) - 1))
    top  = countScore $ genericTake y (ys !! ((fromIntegral x) - 1))
    bot = countScore $ reverse $ genericDrop (y - 1) (ys !! ((fromIntegral x) - 1))

vis :: Ord a => [a] -> Bool
vis xs = all (<(last xs)) $ init xs

countScore :: (Ord a, Num b) => [a] -> b
countScore xs =f dir tree  where
    tree = last xs
    dir = reverse (init xs)

f:: (Ord a, Num b) => [a] -> a -> b
f []     y = 0
f (x:xs) y 
    | y > x = 1 + f xs y
    | otherwise = 1