module World where

import Graphics.Gloss.Interface.IO.Game
import Snake
import Debug.Trace
import System.Random

data World = World {
    snake :: Snake,
    score :: Int,
    food :: Point,
    gen :: StdGen
} deriving (Show)

createWorld :: World
createWorld =
    let _snake = Snake {body = [(0,0)], queueDirection = None, direction = None, len = 1, dead = False }
    in World {snake = _snake, score = 0, food=(2,2), gen=mkStdGen 0}

handleInput :: Event -> World -> World
-- handleInput event _world | trace ("Input handling: " ++ show(event) ++ show(_world)) False = undefined
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

-- TODO: Avoid posibility of food being placed on Snake
moveFood :: World -> World
moveFood _world =
    let
        g0 = gen _world
        (x, g1) = randomR (-8,7 :: Int) g0
        (y, g2) = randomR (-8,7 :: Int) g1
    in
        _world { food = (fromIntegral x, fromIntegral y), gen = g2 }

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


displayFood :: World -> Int -> Picture
-- displayFood _snake cellWidth | trace ("Drawing Food: " ++ show _snake ++ show cellWidth) False = undefined
displayFood _world cellWidth =
    let
        x = fst $ food _world
        y = snd $ food _world
        offset_x = fromIntegral cellWidth * x + fromIntegral(div cellWidth 2)
        offset_y = fromIntegral cellWidth * y + fromIntegral(div cellWidth 2)
    in translate offset_x offset_y $ color blue $ rectangleSolid (fromIntegral cellWidth) (fromIntegral cellWidth)



drawWorld :: World -> Int -> Picture
-- drawWorld _world _cellWidth | trace ("drawWorld(): " ++ (show _world) ) False = undefined
drawWorld _world _cellWidth =
    pictures $ displayFood _world _cellWidth : [displaySnake (snake _world) _cellWidth]
