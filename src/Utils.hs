module Utils where

data Direction = North | South | East | West | None deriving(Enum, Show, Eq)

sumTuple :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumTuple (a,b) (c,d) = (a+c, b+d)
