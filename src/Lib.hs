module Lib
    ( showWindow
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import System.Random

import World
import Snake
import Utils
import Display

-- Window Size
width = 400 :: Int
height = 400 :: Int

-- Window Coordinates
top = div height 2 :: Int
right = div width 2 :: Int
bottom = -top :: Int
left = -right :: Int

cellWidth = 25 :: Int
tickRate = 10 :: Int

window :: Display
window = InWindow "Snake" (width, height) (100, 100)

background :: Color
background = black

point :: (Int, Int) -> (Float, Float)
point (x,y) = (fromIntegral x, fromIntegral y)

grid :: [Picture]
grid = [ line [point(x, top), point(x, bottom)]
                | x <- [left, left+cellWidth .. right]]
        ++ [ line [point(left, x), point(right, x)]
                | x <- [bottom, bottom+cellWidth .. top]]

colorGrid :: [Picture]
colorGrid = [color white _line | _line <- grid]

reDraw :: World -> Picture
-- reDraw _world | trace ("Redrawing: " ++ show _world ) False = undefined
reDraw _world = pictures (colorGrid ++ [drawWorld _world cellWidth])

showWindow :: IO ()
showWindow = do
    randomGen <- newStdGen
    play
        window
        background
        tickRate
        (createWorld randomGen)
        reDraw
        handleInput
        stepWorld

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
