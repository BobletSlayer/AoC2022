import Data.List

main = do
    content <- readFile "test.txt"
    let firstPart = map (\(x:_:xs) -> (x,(read xs)::Integer)) $ lines content

    print firstPart

type Head = (Integer,Integer)
type Tail = (Integer,Integer)
--                           head              tail
move :: (Char, Integer) -> (Head,Tail) -> (Head,Tail)
move (_, 0) headAndTail = headAndTail
move (direction, x) (h@(hx, hy),t@(tx,ty)) = let movedHead = (moveHead direction h) in move (direction, (x-1)) (h,movedHead) 

moveHead :: Char -> Head -> Head
moveHead 'U' (x,y) = (x, (y + 1))
moveHead 'D' (x,y) = (x, (y - 1))
moveHead 'L' (x,y) = ((x - 1), y)
moveHead 'R' (x,y) = ((x + 1), y)

-- moveTail :: 