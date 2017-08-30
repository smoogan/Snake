module Lib
    ( showWindow
    ) where

import Graphics.Gloss
import Debug.Trace

import World

width :: Int
width = 400

height :: Int
height = 400

top :: Float
top = (fromIntegral height)/2

left :: Float
left = -(fromIntegral width)/2

bottom :: Float
bottom = -top

right :: Float
right = -left

cellWidth :: Float
cellWidth = 25

window :: Display
window = InWindow "Snake" (width, height) (10, 10)

background :: Color
background = black

world :: World
world = createWorld

grid :: [Picture]
grid = [ color white $ line [(x, top), (x, bottom)] | x <- [left, left+cellWidth .. right]]
        ++ [ color white $ line [(left, x), (right, x)] | x <- [bottom, bottom+cellWidth .. top]]

reDraw :: World -> Picture
-- reDraw _world | trace ("Redrawing: " ++ show _world ) False = undefined
reDraw _world = pictures (grid ++ [drawWorld _world cellWidth])

showWindow :: IO ()
showWindow = play window background 10 world reDraw handleInput stepWorld
