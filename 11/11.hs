{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.List.Split
import Debug.Trace

main = do
    content <- readFile "test.txt"
    let textMonkeys = splitOn "\n\n" content
    let monkeys = map textToMonkey textMonkeys 
    let round =  playRound $ map addCounter monkeys 

    -- let round =  map addCounter monkeys

    print monkeys
    print round



addCounter :: Monkey -> Monkey
addCounter (Monkey items op test tr fl c) = Monkey items op test tr fl (c + (genericLength items))

round :: [Monkey] -> [Monkey] -> [Monkey]
round [] ms = ms
round [m:ms] bs = 

-- playRound :: [Monkey] -> [(Integer,Integer)]
-- playRound []                    = []
-- playRound ((Monkey items op test tr fl c):xs) = (foldr (\x y ->let i = (div (op x) 3) in ((if test i then tr else fl),(i)):y) [] items)++(playRound xs)

playRound :: Monkey -> [(Integer,Integer)]
playRound (Monkey items op test tr fl c) = (foldr (\x y ->let i = (div (op x) 3) in ((if test i then tr else fl),(i)):y) [] items)








data Monkey = Monkey [Integer] Operation Test TrueTest FalseTest Counter
instance Show Monkey where
    show (Monkey items _ _ true false counter) = "Monkey" ++ (show items) ++ " " ++ (show true) ++ " "++ (show false)++ " " ++ (show counter)
type Operation = (Integer -> Integer)
type Test = (Integer -> Bool)
type TrueTest =  Integer
type FalseTest =  Integer
type Counter = Integer


textToMonkey :: String -> Monkey
textToMonkey xs = Monkey monkeyItems monkeyOperaion monkeyTest monkeyTrue monkeyFalse 0 where
    (x:items:operation:test:true:false:[]) = lines xs
    monkeyItems = map (\num-> (read num)::Integer) $ splitOn "," $ tail $ dropWhile (/=':') items 
    monkeyOperaion = toOperation ( tail $ dropWhile (/='=') operation )
    monkeyTest = toTest test
    monkeyTrue = (read . last $ splitOn " " true)::Integer
    monkeyFalse = (read . last $ splitOn " " false)::Integer
    toOperation :: String -> (Integer -> Integer)
    toOperation " old + old"                          = (\x-> x + x)
    toOperation " old * old"                          = (\x-> x * x)
    toOperation (stripPrefix " old + " -> Just value) = (\x -> x + (read value)) 
    toOperation (stripPrefix " old * " -> Just value) = (\x -> x * (read value)) 
    toTest :: String -> (Integer -> Bool)
    toTest (stripPrefix "  Test: divisible by " -> Just value) = (\x -> (mod x (read value)) == 0)


