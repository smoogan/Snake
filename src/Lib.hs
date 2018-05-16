module Lib
    ( showWindow
    ) where

import Graphics.Gloss
import Debug.Trace

import World

width = 400 :: Int
height = 400 :: Int
top = div height 2 :: Int
right = div width 2 :: Int
bottom = -top :: Int
left = -right :: Int

cellWidth :: Int
cellWidth = 25

window :: Display
window = InWindow "Snake" (width, height) (10, 10)

background :: Color
background = black

world :: World
world = createWorld

grid :: [Picture]
grid = [ color white $ line [(fromIntegral x, fromIntegral top), (fromIntegral x, fromIntegral bottom)] | x <- [left, left+cellWidth .. right]]
        ++ [ color white $ line [(fromIntegral left, fromIntegral x), (fromIntegral right, fromIntegral x)] | x <- [bottom, bottom+cellWidth .. top]]

reDraw :: World -> Picture
-- reDraw _world | trace ("Redrawing: " ++ show _world ) False = undefined
reDraw _world = pictures (grid ++ [drawWorld _world cellWidth])

showWindow :: IO ()
showWindow = play window background 10 world reDraw handleInput stepWorld
