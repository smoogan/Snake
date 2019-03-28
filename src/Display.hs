module Display where

import Graphics.Gloss
import Snake
import World

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
