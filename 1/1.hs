import System.IO  
import Control.Monad
import Data.List
import Data.List.Split

main = do  
        content <- readFile "1.txt"
        let firstPart = foldr (\x y-> if x > y then x else y) 0 $ caloriesByElves content
        let secondPart = (\(x:y:z:xs) -> (x + y + z)) $ reverse . sort $ caloriesByElves content
        print firstPart
        print secondPart

caloriesByElves :: String -> [Int]
caloriesByElves x = map (\x->sum . map (read) $ splitOn "\n" x) $ splitOn "\n\n" x