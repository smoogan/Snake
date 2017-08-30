module Snake where

import Graphics.Gloss
import Debug.Trace

data Direction = North | South | East | West | None deriving(Enum, Show, Eq)

data Snake = Snake { 
    body :: [Point],
    direction :: Direction,
    len :: Int
} deriving (Show)


sumTuple :: (Float, Float) -> (Float, Float) -> (Float, Float)
-- sumTuple a b | trace ("Adding tuples: " ++ show a ++ " and " ++ show b) False = undefined
sumTuple (a,b) (c,d) = (a+c, b+d)

moveSnake :: Snake -> Snake
-- moveSnake s | trace ("Moving Snake: " ++ show s) False = undefined
moveSnake _snake = 
    if onMap newPosition 
        then _snake { body = [newPosition] ++ init (body _snake)  }
        else _snake
        where newPosition
                | direction _snake == North = sumTuple (head $ body _snake) ( 0, 1)
                | direction _snake == South = sumTuple (head $ body _snake) ( 0,-1)
                | direction _snake == East  = sumTuple (head $ body _snake) ( 1, 0)
                | direction _snake == West  = sumTuple (head $ body _snake) ( -1, 0)
                | otherwise = head $ body _snake
              onMap (x, y) = x < 8 && x > -9 && y < 8 && y > -9


changeDirection :: Snake -> Direction -> Snake
changeDirection _snake _direction = _snake {direction = _direction}

growSnake :: Snake -> Snake
growSnake _snake = _snake { len = (len _snake) + 1}

displaySnake :: Snake -> Float -> Picture
-- displaySnake _snake cellWidth | trace ("Drawing Snake: " ++ show _snake ++ show cellWidth) False = undefined
displaySnake _snake cellWidth = pictures $ map (displayCell cellWidth) (body _snake)

displayCell :: Float -> Point -> Picture
displayCell cellWidth point = 
    let
        offset = fromInteger $ floor (cellWidth / 2)
        x = fst point
        y = snd point
    in
        translate (cellWidth * x + offset) (cellWidth * y + offset) $ color red $ rectangleSolid cellWidth cellWidth
