module Snake where

import Graphics.Gloss
import Debug.Trace
import Utils

data Snake = Snake {
    body :: [Point],
    queueDirection :: Direction,
    direction :: Direction,
    len :: Int,
    dead :: Bool
} deriving (Show)



moveSnake :: Snake -> Snake
moveSnake _snake
    | direction _snake == None = _snake
    | onMap newPosition && not (snakeOccupiesPoint _snake newPosition)
        = _snake { body = newPosition : init (body _snake) }
    | otherwise = _snake { dead = True }
    where newPosition
            | direction _snake == North = sumTuple (head $ body _snake) ( 0, 1)
            | direction _snake == South = sumTuple (head $ body _snake) ( 0,-1)
            | direction _snake == East  = sumTuple (head $ body _snake) ( 1, 0)
            | direction _snake == West  = sumTuple (head $ body _snake) ( -1, 0)
            | otherwise = head $ body _snake
          onMap (x, y) = x < 8 && x > -9 && y < 8 && y > -9

snakeOccupiesPoint :: Snake -> (Float, Float) -> Bool
snakeOccupiesPoint _snake (x, y) = (x, y) `elem` body _snake

changeDirection :: Snake -> Direction -> Snake
changeDirection _snake _direction =
    if dead _snake
        || _direction == direction _snake
        || (_direction == North && direction _snake == South)
        || (_direction == South && direction _snake == North)
        || (_direction == East && direction _snake == West)
        || (_direction == West && direction _snake == East)
        then _snake
        else _snake {queueDirection = _direction}

growSnake :: Snake -> Snake
growSnake _snake = _snake { len = len _snake + 1}
