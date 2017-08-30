module World where

import Graphics.Gloss.Interface.IO.Game
import Snake
import Debug.Trace

data World = World {
    snake :: Snake,
    score :: Int,
    food :: Point
} deriving (Show)

createWorld :: World
createWorld = 
    let _snake = Snake {position = (0,0), direction = None, len = 1}
    in World {snake = _snake, score = 0, food=(2,2)}

handleInput :: Event -> World -> World
handleInput event _world
    | EventKey (SpecialKey KeyUp) Down _ (_, _) <- event
    = _world {snake = changeDirection (snake _world) North}

    | EventKey (SpecialKey KeyDown) Down _ (_, _) <- event
    = _world { snake = changeDirection (snake _world) South }

    | EventKey (SpecialKey KeyLeft) Down _ (_, _) <- event
    = _world { snake = changeDirection (snake _world) West }

    | EventKey (SpecialKey KeyRight) Down _ (_, _) <- event
    = _world { snake = changeDirection (snake _world) East }
handleInput _ s = s

moveFood :: World -> World
moveFood _world =
    let
        newPoint = (0,0) -- Randomize this
    in
        _world { food = newPoint }

stepWorld :: Float -> World -> World
stepWorld _ _world | trace ("Stepping World: " ++ show (_world)) False = undefined
stepWorld _ _world = 
    let
        movedSnake = moveSnake $ snake _world
    in
        if position movedSnake == food _world
            then
                moveFood $ _world { snake = movedSnake { len = (len movedSnake) + 1} }
            else
                _world { snake = movedSnake }


drawWorld :: World -> Float -> Picture
-- drawWorld _world _cellWidth | trace ("drawWorld(): " ++ (show _world) ) False = undefined
drawWorld _world _cellWidth = displaySnake (snake _world) _cellWidth
