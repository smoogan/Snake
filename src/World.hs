module World where

import Graphics.Gloss.Interface.IO.Game
import Snake
import Debug.Trace

data World = World {
    snake :: Snake,
    score :: Int
} deriving (Show)

createWorld :: World
createWorld = 
    let _snake = Snake {position = (0,0), direction = None, len = 1}
    in World {snake = _snake, score = 0}

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

stepWorld :: Float -> World -> World
stepWorld _ _world | trace ("Stepping World: " ++ show (snake _world)) False = undefined
stepWorld _ _world = _world { snake = moveSnake $ snake _world }

drawWorld :: World -> Float -> Picture
drawWorld _world _cellWidth | trace ("drawWorld(): " ++ show _world ) False = undefined
drawWorld _world _cellWidth = displaySnake (snake _world) _cellWidth
