module World where

import Snake
import Debug.Trace
import System.Random
import Graphics.Gloss.Interface.IO.Game

import Utils

data World = World {
    snake :: Snake,
    score :: Int,
    food :: Point,
    gen :: StdGen
} deriving (Show)

createWorld :: StdGen -> World
createWorld _g = World {
        score = 0,
        food = (2,2),
        snake = Snake {
            body = [(0,0)],
            queueDirection = None,
            direction = None,
            len = 1,
            dead = False },
        gen = _g }

-- TODO: Avoid posibility of food being placed on Snake
moveFood :: World -> World
moveFood _world =
    let
        emptySpaces = [
            (x,y) |
                x <- [-8..7],
                y <- [-8..7],
                not $ snakeOccupiesPoint (snake _world) (x,y) ]
        g0 = gen _world
        listLen = length emptySpaces
        (pointIndex, newGen) = randomR (0, listLen-1 :: Int) g0
        (x, y) = emptySpaces !! pointIndex
    in
        _world { food = (x, y), gen = newGen }

stepWorld :: Float -> World -> World
-- stepWorld _world | trace ("Stepping World: " ++ show (_world)) False = undefined
stepWorld _time _world =
    let
        tailCell = last . body $ snake _world
        movedSnake = moveSnake $ (snake _world) {direction = queueDirection (snake _world)}
    in
        if head (body movedSnake) == food _world
            then
                moveFood _world {
                    snake = movedSnake {
                        len = len movedSnake + 1,
                        body = body movedSnake ++ [tailCell]
                    }
                }
            else
                _world { snake = movedSnake }
