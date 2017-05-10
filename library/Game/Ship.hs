{-# LANGUAGE NamedFieldPuns #-}

module Game.Ship (Ship, new, render, tick) where

import Data.Function ((&))
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture

import qualified Game.Screen as Screen

shipCollisionRadius :: Float
shipCollisionRadius = 16

data Ship = Ship
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , collisionRadius :: Float
  , dead :: Bool
  }

new :: (Float, Float) -> Float -> Ship
new pos speed' =
  Ship
    { pos = pos
    , speed = speed'
    , angle = 0
    , collisionRadius = shipCollisionRadius
    , dead = False
    }

render :: Ship -> Picture
render Ship{angle, collisionRadius, pos} =
  Picture.polygon [(-1,-1), (0,1), (1, -1)]
    & Picture.scale collisionRadius collisionRadius
    & Picture.rotate angle
    & uncurry Picture.translate (Screen.fromTopLeft pos)
    & Picture.color Color.azure 

tick :: Ship -> Ship
tick =
  id