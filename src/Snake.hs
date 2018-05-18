module Snake where

import Graphics.Gloss
import Debug.Trace

data Direction = North | South | East | West | None deriving(Enum, Show, Eq)

data Snake = Snake {
    body :: [Point],
    queueDirection :: Direction,
    direction :: Direction,
    len :: Int,
    dead :: Bool
} deriving (Show)


sumTuple :: (Float, Float) -> (Float, Float) -> (Float, Float)
-- sumTuple a b | trace ("Adding tuples: " ++ show a ++ " and " ++ show b) False = undefined
sumTuple (a,b) (c,d) = (a+c, b+d)

moveSnake :: Snake -> Snake
-- moveSnake s | trace ("Moving Snake: " ++ show s) False = undefined
moveSnake _snake
    | direction _snake == None = _snake
    | onMap newPosition && not (occupied newPosition) = _snake { body = newPosition : init (body _snake)  }
    | otherwise = _snake { dead = True }
    where newPosition
            | direction _snake == North = sumTuple (head $ body _snake) ( 0, 1)
            | direction _snake == South = sumTuple (head $ body _snake) ( 0,-1)
            | direction _snake == East  = sumTuple (head $ body _snake) ( 1, 0)
            | direction _snake == West  = sumTuple (head $ body _snake) ( -1, 0)
            | otherwise = head $ body _snake
          onMap (x, y) = x < 8 && x > -9 && y < 8 && y > -9
          occupied (x, y) = (x, y) `elem` body _snake


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

displaySnake :: Snake -> Int -> Picture
-- displaySnake _snake cellWidth | trace ("Drawing Snake: " ++ show _snake ++ show cellWidth) False = undefined
displaySnake _snake cellWidth = pictures $ map (displayCell cellWidth) (body _snake)

displayCell :: Int -> Point -> Picture
displayCell cellWidth point =
    let
        x = fst point
        y = snd point
        offset_x = fromIntegral cellWidth * x + fromIntegral(div cellWidth 2)
        offset_y = fromIntegral cellWidth * y + fromIntegral(div cellWidth 2)
    in
        translate offset_x offset_y $ color red $ rectangleSolid (fromIntegral cellWidth) (fromIntegral cellWidth)
