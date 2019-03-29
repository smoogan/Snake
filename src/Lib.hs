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

cellWidth :: Int
cellWidth = 25

tickRate :: Int
tickRate = 10

window :: Display
window = InWindow "Snake" (width, height) (10, 10)

background :: Color
background = black

grid :: [Picture]
grid = [ color white $ line [(fromIntegral x, fromIntegral top), (fromIntegral x, fromIntegral bottom)] | x <- [left, left+cellWidth .. right]]
        ++ [ color white $ line [(fromIntegral left, fromIntegral x), (fromIntegral right, fromIntegral x)] | x <- [bottom, bottom+cellWidth .. top]]

reDraw :: World -> Picture
-- reDraw _world | trace ("Redrawing: " ++ show _world ) False = undefined
reDraw _world = pictures (grid ++ [drawWorld _world cellWidth])

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
