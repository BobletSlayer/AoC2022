import Data.List

main = do
    content <- readFile "9.txt"
    let moves = map (\(x:_:xs) -> (x,(read xs)::Integer)) $ lines content
    let tailPositions = fst $ foldl (\x y -> move y x) ([],((0,0),(0,0))) moves
    let uniquePositions = nub tailPositions
    let firstPart = genericLength uniquePositions

    let movedKnots = foldl (\x y -> moveMultiple y x) ([],((0,0),(take 9 (repeat (0,0))))) moves
    let secondPart = genericLength . nub $ fst  movedKnots

    print firstPart
    print secondPart

type Head = (Integer,Integer)
type Tail = (Integer,Integer)

move :: (Char, Integer) -> ([Tail],(Head,Tail)) -> ([Tail],(Head,Tail))
move (_, 0) (ts,(h,t)) = ((t:ts), (h, t))
move (direction, count) (tails,(head,tail)) = moved where
    movedHead     = moveHead direction head
    movedTail     = moveTail movedHead tail
    tailPositions = (tail:tails)
    moved         = move (direction, (count - 1)) (tailPositions, (movedHead, movedTail))

moveHead :: Char -> Head -> Head
moveHead 'U' (x,y) = (x, (y + 1))
moveHead 'D' (x,y) = (x, (y - 1))
moveHead 'L' (x,y) = ((x - 1), y)
moveHead 'R' (x,y) = ((x + 1), y)

moveTail :: Head -> Tail -> Tail
moveTail (hx,hy) (tx, ty) 
    | (tx == hx) && (hy > (ty + 1)) = (tx, (ty + 1))
    | (tx == hx) && (hy < (ty - 1)) = (tx, (ty - 1))
    | (ty == hy) && (hx > (tx + 1)) = ((tx + 1), ty)
    | (ty == hy) && (hx < (tx - 1)) = ((tx - 1), ty)
    -- 
    | (hx > tx)  && (hy > (ty + 1)) = ((tx + 1),(ty + 1))
    | (hx < tx)  && (hy > (ty + 1)) = ((tx - 1),(ty + 1))
    | (hx > tx)  && (hy < (ty - 1)) = ((tx + 1),(ty - 1))
    | (hx < tx)  && (hy < (ty - 1)) = ((tx - 1),(ty - 1))
     --
    | (hy > ty)  && (hx > (tx + 1)) = ((tx + 1),(ty + 1))
    | (hy < ty)  && (hx > (tx + 1)) = ((tx + 1),(ty - 1))
    | (hy > ty)  && (hx < (tx - 1)) = ((tx - 1),(ty + 1))
    | (hy < ty)  && (hx < (tx - 1)) = ((tx - 1),(ty - 1))
    | otherwise = (tx,ty)

moveMultiple :: (Char, Integer) -> ([Tail],(Head, [Tail])) -> ([Tail],(Head, [Tail]))
moveMultiple  (_, 0) hts = hts
moveMultiple (direction, count) (lastPositions,(head,knots)) = moved where
    movedHead = moveHead direction head
    movedKnots = moveKnots movedHead knots
    lastKnotPosition = last movedKnots
    moved = moveMultiple (direction, (count - 1)) ((lastKnotPosition:lastPositions), (movedHead, movedKnots))

moveKnots :: Head -> [Tail] -> [Tail]
moveKnots head [] = []
moveKnots head (tail:tails) = movedTail:(moveKnots movedTail tails) where
    movedTail = moveTail head tail