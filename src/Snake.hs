module Snake where

import Graphics.Gloss
import Debug.Trace

data Direction = North | South | East | West | None deriving(Enum, Show, Eq)

data Snake = Snake { 
    position :: Point,
    direction :: Direction,
    len :: Int
} deriving (Show)

-- snake :: Snake
-- snake = Snake {position=(0,0), direction=None, len=1}

sumTuple :: (Float, Float) -> (Float, Float) -> (Float, Float)
-- sumTuple a b | trace ("Adding tuples: " ++ show a ++ " and " ++ show b) False = undefined
sumTuple (a,b) (c,d) = (a+c, b+d)

moveSnake :: Snake -> Snake
-- moveSnake s | trace ("Moving Snake: " ++ show s) False = undefined
moveSnake _snake = 
    if onMap newPosition 
        then _snake { position = newPosition }
        else _snake
        where newPosition
                | direction _snake == North = sumTuple (position _snake) ( 0, 1)
                | direction _snake == South = sumTuple (position _snake) ( 0,-1)
                | direction _snake == East  = sumTuple (position _snake) ( 1, 0)
                | direction _snake == West  = sumTuple (position _snake) ( -1, 0)
                | otherwise = position _snake
              onMap (a, b) = a < 8 && a > -9 && b < 8 && b > -9


changeDirection :: Snake -> Direction -> Snake
changeDirection _snake _direction = _snake {direction = _direction}

displaySnake :: Snake -> Float -> Picture
-- displaySnake _snake cellWidth | trace ("Drawing Snake: " ++ show _snake ++ show cellWidth) False = undefined
displaySnake _snake cellWidth = 
    let 
        offset = fromInteger $ floor (cellWidth / 2)
        y = snd $ position _snake
        x = fst $ position _snake
    in translate (cellWidth * x + offset) (cellWidth * y + offset) $ color red $ rectangleSolid cellWidth cellWidth
