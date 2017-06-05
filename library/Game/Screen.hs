module Game.Screen
  ( dimensions
  , fromTopLeft
  , screenOffset
  ) where

dimensions :: (Int, Int)
dimensions =
  (500, 500)

fromTopLeft :: (Float, Float) -> (Float, Float)
fromTopLeft (x, y) =
  let
    (width, height) = dimensions
  in
    ( x - (fromIntegral width / 2)
    , (fromIntegral height / 2) - y
    )

screenOffset :: (Int, Int)
screenOffset =
  (250, 250)
