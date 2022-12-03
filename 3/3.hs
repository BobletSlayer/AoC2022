import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main = do  
    content <- readFile "3.txt"
    let firstPart  = sum . map (itemValue . findDuplicate . splitInHalf) $ splitOn "\n" content
    let secondPart = sum . map (itemValue . commonItem) $ groupThreeLines $ splitOn "\n" content

    print firstPart
    print secondPart

priority :: [(Char,Integer)]
priority = (zip ['a'..'z'] [1..])++(zip ['A'..'Z'] [27..])

splitInHalf :: String -> (String, String)
splitInHalf x = ((take len x),(drop len x)) where
    len = (genericLength x) `div` 2

findDuplicate :: (String, String) -> Char
findDuplicate ((x:xs),y) 
    | any (==x) y = x
    | otherwise   = findDuplicate (xs,y)
findDuplicate x   = error "sus"

itemValue :: Char -> Integer
itemValue item = snd $ fromMaybe (' ', 0) $ find (\x-> item == fst x) priority --cursed 

groupThreeLines :: [String] -> [[String]]
groupThreeLines []         = []
groupThreeLines (a:b:c:xs) = [a,b,c] : groupThreeLines xs

commonItem :: [String] -> Char
commonItem ((x:xs):ys:zs:[])
    | any (==x) ys && any (==x) zs = x
    | otherwise                    = commonItem (xs:ys:zs:[])
commonItem x          = error "mogus"