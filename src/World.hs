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


-- Places the food on a random spot not occupied by Snake
moveFood :: World -> World
moveFood _world =
    let
        -- Get a list of all unoccupied spots on the grid
        emptySpaces = [
            (x,y) |
                x <- [-8..7],
                y <- [-8..7],
                not $ snakeOccupiesPoint (snake _world) (x,y) ]

        -- Pick a random point from the list
        g0 = gen _world
        listLen = length emptySpaces
        (pointIndex, newGen) = randomR (0, listLen-1 :: Int) g0
        (x, y) = emptySpaces !! pointIndex
    in
        _world { food = (x, y), gen = newGen }


-- Move the world forward one iteration (i.e. Snake takes a step forward)
stepWorld :: Float -> World -> World
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
