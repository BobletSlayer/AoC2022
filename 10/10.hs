{-# LANGUAGE ViewPatterns #-}

import Data.List

main = do
    content <- readFile "10.txt"

    let commands = foldr (\x y -> (lineToCommand x ) ++ y) [] $ lines content
    let executed = map (\cycle -> (cycle) * (foldr (\x y -> execute x y) 1 $ genericTake (cycle - 1) commands)) [20,60..220]
    let positions = map (\cycle -> (foldr (\x y -> execute x y) 1 $ genericTake (cycle - 1) commands)) [41,81..240]
    let firstPart = sum executed

    let lines = map (\(y,x) -> (draw x 0 (y))) $ zip (1:positions) (crtLines commands)

    print firstPart
    putStr $ foldr (\y x -> y++"\n"++x) [] lines

data Command = Noop | Addx Integer deriving Show

lineToCommand :: String -> [Command]
lineToCommand "noop" = [Noop]
lineToCommand (stripPrefix "addx " -> Just value) = [Noop, (Addx ((read value)::Integer))]

execute :: Command -> Integer -> Integer
execute Noop v     = v
execute (Addx x) v = v + x

crtLines :: [Command] -> [[Command]]
crtLines [] = []
crtLines xs = (genericTake 40 xs):(crtLines (genericDrop 40 xs)) 

draw :: [Command] -> Integer -> Integer -> String
draw []     _     _        = [] 
draw (c:cs) count position 
    | (count == position || count == (position - 1) || count == position + 1) = '#':(draw cs (count + 1) (execute c position)) 
    | otherwise                                                               = ' ':(draw cs (count + 1) (execute c position)) 